package io.tvc.graphql.transform

import atto.syntax.parser._
import cats.Monad
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.apply._
import io.tvc.graphql.parsing.CommonModel.Type.{ListType, NamedType, NonNullType}
import io.tvc.graphql.parsing.CommonModel.{Name, Type}
import io.tvc.graphql.parsing.QueryModel.{OperationDefinition, SelectionSet}
import io.tvc.graphql.parsing.QueryParser.operationDefinition
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition._
import io.tvc.graphql.parsing.SchemaModel.{FieldDefinition, Schema, TypeDefinition}
import io.tvc.graphql.parsing.SchemaParser.{load, schema}
import io.tvc.graphql.transform.TypeChecker.TypeError.{MissingField, MissingType, ScalarWithFields}

import scala.annotation.tailrec

/**
  * Here we take a query and a schema
  * and attempt to find types for all of the fields in the query,
  * the idea is we'll output a tree structure of nested typed objects
  */
object TypeChecker extends App {

  /**
    * https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
    */
  val predefinedTypes: List[TypeDefinition] =
    List("Int", "Float", "Boolean", "String", "ID").map(t => ScalarTypeDefinition(None, Name(t), List.empty))

  case class FieldName(alias: Option[String], value: String) {
    override def toString: String = (alias.toList :+ value).mkString(":")
  }

  /**
   * Field types in GraphQL can be both concrete objects and also interfaces
   * As such we need to abstract them into a common complex type
   */
  case class ComplexType(name: Name, fields: List[FieldDefinition])

  object ComplexType {

    def unapply(td: ResolvedType): Option[ComplexType] =
      td.definition match {
        case o: ObjectTypeDefinition => Some(ComplexType(o.name, o.values))
        case o: InterfaceTypeDefinition => Some(ComplexType(o.name, o.values))
        case _ => None
      }
  }

  object ScalarType {

    def unapply(td: ResolvedType): Option[Type] =
      td.definition match {
        case _: EnumTypeDefinition => Some(td.ref)
        case _: UnionTypeDefinition => Some(td.ref)
        case _: ScalarTypeDefinition => Some(td.ref)
        case _ => None
      }
  }

  /**
   * A path is a fully qualified path to a name
    * i.e. foo.bar.baz
   */
  case class Path(values: Vector[FieldName]) {
    override def toString: String = values.mkString(".")
  }

  /**
   * The object model is what we create by combining a query and a schema,
    * it produces a nested JSON like tree structure of fields + objects
   */
  sealed trait Output[A]

  object Output {
    case class Field[A](name: A, fieldType: Type) extends Output[A]
    case class Object[A](name: A, fields: List[Field[A]]) extends Output[A]
  }

  sealed trait TypeError

  object TypeError {
    case class MissingType(name: String) extends TypeError
    case class MissingField(name: String) extends TypeError
    case class ScalarWithFields(scalar: Type, field: Path) extends TypeError
  }

  type OrMissing[A] = Either[TypeError, A]

  /**
   * Given a type, which may be non nullable or a list
   * or some nested variant thereof, extract its name
   */
  @tailrec
  def typeName(typ: Type): Name =
    typ match {
      case NamedType(value) => value
      case ListType(value) => typeName(value)
      case NonNullType(value) => typeName(value)
    }

  /**
   * Given a complex type and a field name
   * try to find a field with the same name defined in the type
   */
  def findFieldType(field: FieldName, obj: ComplexType): OrMissing[Type] =
    obj.fields
      .find(_.name.value == field.value)
      .toRight(MissingField(s"${obj.name}.${field.value}"))
      .map(_.`type`)

  /**
    * A resolved type is a reference to a particular type (with modifiers like non null etc)
    * and the actual definition of that type (scalar, object etc)
    */
  case class ResolvedType(ref: Type, definition: TypeDefinition)

  def resolveType(schema: Schema, tpe: Type): OrMissing[ResolvedType] =
    (predefinedTypes ++ schema).find(_.name == typeName(tpe))
      .map(t => ResolvedType(ref = tpe, definition = t))
      .toRight(MissingType(typeName(tpe).value))

  /**
    * Given a list of schema types
    * try to find a root ObjectTypeDefinition
    */
  def root(schema: Schema): OrMissing[ResolvedType] =
    resolveType(schema, NamedType(Name("Query")))

  /**
    * Given a selectionSet and a function operating on the path to a leaf,
    * descend through the set and all nested sets and apply the function to all leaves
    */
  def traverse[A](selectionSet: SelectionSet)(f: Path => A): List[A] =
    Monad[List].tailRecM(Path(Vector.empty) -> selectionSet) { case (ns, set) =>
      set.fields.map(f => FieldName(f.alias.map(_.value), f.name.value) -> f.selectionSet).flatMap {
        case (fieldName, Some(child)) =>
          List(Right(f(Path(ns.values :+ fieldName))), Left(Path(ns.values :+ fieldName) -> child))
        case (fieldName, None) =>
          List(Right(f(Path(ns.values :+ fieldName))))
      }
    }

  /**
    * Given a schema and the fully qualified name of a query field
    * dig through the schema to find out what the type of the field should be
    */
  def createFields(schema: Schema)(path: Path): OrMissing[Output.Field[Path]] =
    root(schema).flatMap { root =>
      Monad[OrMissing].tailRecM(path -> root) {
        case (Path(Vector()), obj) =>
          Right(Right(Output.Field(path, obj.ref)))
        case (p, ScalarType(obj)) =>
          Left(ScalarWithFields(obj, p))
        case (Path(p +: ps), ComplexType(obj)) =>
          for {
            field <- findFieldType(p, obj)
            fieldType <- resolveType(schema, field)
          } yield Left(Path(ps) -> fieldType)
      }
  }

  def something(schema: Schema, query: OperationDefinition): Unit = {
    traverse(query.selectionSet)(f => f -> createFields(schema)(f)).foreach(println)
  }

 (
   schema.parseOnly(load("/schemas/schema.idl")).option,
   operationDefinition.parseOnly(load("/queries/query.graphql")).option
 ).mapN(something)
}
