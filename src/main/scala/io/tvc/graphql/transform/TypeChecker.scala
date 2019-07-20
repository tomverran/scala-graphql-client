package io.tvc.graphql.transform

import cats.Monad
import cats.data.NonEmptyList
import cats.instances.either._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.traverse._
import io.tvc.graphql.parsing.CommonModel
import io.tvc.graphql.parsing.CommonModel.Type.{ListType, NamedType, NonNullType}
import io.tvc.graphql.parsing.CommonModel.{Name, Type}
import io.tvc.graphql.parsing.QueryModel.{OperationDefinition, SelectionSet}
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition._
import io.tvc.graphql.parsing.SchemaModel.{FieldDefinition, Schema, TypeDefinition}
import io.tvc.graphql.transform.TypeChecker.TypeError.{MissingField, MissingType, OrMissing, ScalarWithFields}

import scala.annotation.tailrec

/**
  * Here we take a query and a schema and produce a QueryResult AST.
  * This is a flat structure of root level complex types used by the query
  */
object TypeChecker {

  /**
    * https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
    */
  val predefinedTypes: List[TypeDefinition] =
    List("Int", "Float", "Boolean", "String", "ID").map(t => ScalarTypeDefinition(None, Name(t), List.empty))

  /**
   * A path is a fully qualified path to a name
    * i.e. foo.bar.baz
   */
  case class Path(values: Vector[FieldName]) {
    override def toString: String = values.mkString(".")
    def last: FieldName = values.lastOption.getOrElse(FieldName(None, "Root"))
    def up: Path = Path(values.dropRight(1))
  }

  /**
    * The name of a field in the output AST,
    * we include alias information for deduplication
    */
  case class FieldName(alias: Option[String], value: String) {
    override def toString: String = (alias.toList :+ value).mkString(":")
  }

  /**
    * A modifier for a type,
    * i.e. the type is a list type etc.
    * Modifiers should be applied recursively in order
    */
  sealed trait TypeModifier

  object TypeModifier {
    case object ListType extends TypeModifier
    case object NonNullType extends TypeModifier
  }

  /**
    * Represents a type in the output QueryResult AST
    * We unwrap the nested QueryModel.type for later simplicity
    */
  case class TypeRef(name: String, modifiers: List[TypeModifier])

  object TypeRef {

    private def create(tpe: Type, mods: Vector[TypeModifier]): TypeRef =
      tpe match {
        case NonNullType(t) => create(t, mods :+ TypeModifier.NonNullType)
        case ListType(t) => create(t, mods :+ TypeModifier.ListType)
        case NamedType(n) => TypeRef(n.value, mods.toList)
      }

    def fromType(tpe: Type): TypeRef =
      create(tpe, Vector.empty)
  }

  /**
   * TypeChecking essentially consists of going through the query
   * and sticking together schema + query information into the below types
   */
  object Resolved {
    case class Type(ref: CommonModel.Type, definition: TypeDefinition)
    case class Field(name: FieldName, resolvedType: Type)
    case class Object(name: FieldName, resolvedType: Type, fields: List[Field])
  }




  sealed trait TypeError

  object TypeError {
    type OrMissing[A] = Either[TypeError, A]
    case class MissingType(name: String) extends TypeError
    case class MissingField(name: String) extends TypeError
    case class ScalarWithFields(scalar: Type, field: Path) extends TypeError
  }

  /**
   * Field types in GraphQL can be both concrete objects and also interfaces
   * As such we need to abstract them into a common complex type
   */
  private case class ComplexType(name: Name, fields: List[FieldDefinition])

  private object ComplexType {

    def unapply(td: Resolved.Type): Option[ComplexType] =
      td.definition match {
        case o: ObjectTypeDefinition => Some(ComplexType(o.name, o.values))
        case o: InterfaceTypeDefinition => Some(ComplexType(o.name, o.values))
        case _ => None
      }
  }

  private object ScalarType {

    def unapply(td: Resolved.Type): Option[Type] =
      td.definition match {
        case _: EnumTypeDefinition => Some(td.ref)
        case _: UnionTypeDefinition => Some(td.ref)
        case _: ScalarTypeDefinition => Some(td.ref)
        case _ => None
      }
  }

  /**
   * Given a type, which may be non nullable or a list
   * or some nested variant thereof, extract its name
   */
  @tailrec
  private def typeName(typ: Type): Name =
    typ match {
      case NamedType(value) => value
      case ListType(value) => typeName(value)
      case NonNullType(value) => typeName(value)
    }

  /**
   * Given a complex type and a field name
   * try to find a field with the same name defined in the type
   */
  private def findFieldType(field: FieldName, obj: ComplexType): OrMissing[Type] =
    obj.fields
      .find(_.name.value == field.value)
      .toRight(MissingField(s"${obj.name.value}.${field.value}"))
      .map(_.`type`)

  /**
    * A resolved type is a reference to a particular type (with modifiers like non null etc)
    * and the actual definition of that type (scalar, object etc)
    */

  /**
    * Try to dig the given type out of the schema,
    * we ignore any type modifiers and just search for a type with the same name
    * but we include the input type including modifiers in the response
    */
  private def resolveType(schema: Schema, tpe: Type): OrMissing[Resolved.Type] =
    (predefinedTypes ++ schema).find(_.name == typeName(tpe))
      .map(t => Resolved.Type(ref = tpe, definition = t))
      .toRight(MissingType(typeName(tpe).value))

  /**
    * Given a list of schema types
    * try to find a root ObjectTypeDefinition
    */
  private def root(schema: Schema): OrMissing[Resolved.Type] =
    resolveType(schema, NamedType(Name("Query")))

  /**
    * Given a selectionSet and a function operating on a list of paths making up the fields
    * descend through the set and all nested sets and apply the function to all leaves
    */
  private def traverse[A](selectionSet: SelectionSet)(f: NonEmptyList[Path] => A): List[A]  =
    Monad[List].tailRecM(selectionSet -> Path(Vector.empty)) { case (set, path) =>
      set.fields.flatMap { field =>
        field.selectionSet.map { nested =>
          Left(nested -> Path(path.values :+ FieldName(field.alias.map(_.value), field.name.value)))
        }
      } ++ NonEmptyList.fromList(set.fields).toList.map { nelFields =>
        Right(
          f(
            nelFields.map { field =>
              Path(path.values :+ FieldName(field.alias.map(_.value), field.name.value))
            }
          )
        )
      }
    }

  /**
    * Given a schema and the fully qualified name of a query field
    * dig through the schema to find out what the type of the field should be
    */
  private def createField(schema: Schema)(path: Path): OrMissing[Resolved.Field] =
    root(schema).flatMap { root =>
      Monad[OrMissing].tailRecM(path -> root) {
        case (Path(Vector()), obj) =>
          Right(Right(Resolved.Field(path.last, obj)))
        case (p, ScalarType(obj)) =>
          Left(ScalarWithFields(obj, p))
        case (Path(p +: ps), ComplexType(obj)) =>
          for {
            field <- findFieldType(p, obj)
            fieldType <- resolveType(schema, field)
          } yield Left(Path(ps) -> fieldType)
      }
    }

  /**
    * Traverse through each selection set in the query
    * and create typed objects from its fields
    */
  def objects(schema: Schema, query: OperationDefinition): OrMissing[List[Resolved.Object]] =
    traverse(query.selectionSet) { fields =>
      for {
        obj <- createField(schema)(fields.head.up)
        fields <- fields.traverse(createField(schema))
      } yield Resolved.Object(obj.name, obj.resolvedType, fields.toList)
    }.sequence
}
