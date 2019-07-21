package io.tvc.graphql.transform

import cats.instances.either._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.traverse._
import io.tvc.graphql.parsing.CommonModel.Type.{ListType, NamedType, NonNullType}
import io.tvc.graphql.parsing.CommonModel.{Name, Type}
import io.tvc.graphql.parsing.QueryModel.{Field, OperationDefinition, SelectionSet}
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition._
import io.tvc.graphql.parsing.SchemaModel.{FieldDefinition, Schema, TypeDefinition}
import io.tvc.graphql.transform.TypeChecker.TypeError.{ExpectedFields, MissingType, OrMissing, Todo}
import io.tvc.graphql.transform.TypeChecker.TypeModifier.fromType

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
    * A cursor field gathers together all the possible information about a field in a SelectionSet -
    * the field's name (including it's alias) - the definition of the field from the schema
    * and the type the field resolves to
    */
  private case class CursorField[A](name: FieldName, definition: FieldDefinition, tpe: A)

  /**
   * A cursor is an object to represent our current place in a tree of query SelectionSets
    * where the previous items are fields with complex types we've delved down into,
    * this is kind of inspired by Circe's cursor
   */
  private case class Cursor(
    schema: Schema,
    prev: Vector[CursorField[ComplexType]],
    focus: CursorField[TypeDefinition])
  {

    def down(field: Field): OrMissing[Cursor] =
      down(FieldName(field.alias.map(_.value), field.name.value))

    def down(field: FieldName): OrMissing[Cursor] =
      for {
        complexType <- ComplexType.fromDefinition(focus.tpe)
        fieldDef <- findFieldDefinition(complexType, field)
        tpe <- findTypeDefinition(schema, fieldDef.`type`)
      } yield Cursor(schema, prev :+ focus.copy(tpe = complexType), CursorField(field, fieldDef, tpe))

    def parent: Option[CursorField[ComplexType]] =
      prev.lastOption
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
    case object NullableType extends TypeModifier

    private def create(tpe: Type, mods: Vector[TypeModifier]): Vector[TypeModifier] =
      tpe match {
        case Type.NonNullType(t) => create(t, mods)
        case a if !mods.lastOption.contains(NullableType) => create(a, mods :+ TypeModifier.NullableType)
        case Type.ListType(a) => create(a, mods :+ TypeModifier.ListType)
        case Type.NamedType(_) => mods
      }

    def fromType(tpe: Type): List[TypeModifier] =
      create(tpe, Vector.empty).toList
  }

  sealed trait TypeTree

  object TypeTree {
    case class Scalar(name: String) extends TypeTree
    case class Enum(name: String, fields: List[String]) extends TypeTree
    case class Union(name: String, fields: List[TypeTree]) extends TypeTree
    case class Object(name: String, fields: List[Field]) extends TypeTree
    case class Field(name: FieldName, `type`: TypeTree, modifiers: List[TypeModifier])
  }

  /**
    * Given a cursor try to turn it into a Scalar type tree by matching on it's current definition
    * Will fail if the definition isn't an enum or a scalar type
    */
  def createFromScalar(c: Cursor): OrMissing[TypeTree] =
    c.focus.tpe match {
      case e: EnumTypeDefinition => Right(TypeTree.Enum(e.name.value, e.values.map(_.value.value.value)))
      case s: ScalarTypeDefinition => Right(TypeTree.Scalar(s.name.value))
      case _ => Left(ExpectedFields(c.focus.tpe.name.value))
    }

  sealed trait TypeError

  object TypeError {
    type OrMissing[A] = Either[TypeError, A]
    case class MissingType(name: String) extends TypeError
    case class MissingField(name: String) extends TypeError
    case class ExpectedFields(field: String) extends TypeError
    case class Todo(name: String) extends TypeError
  }

  /**
   * Field types in GraphQL can be both concrete objects and also interfaces
   * As such we need to abstract them into a common complex type
   */
  private case class ComplexType(name: Name, fields: List[FieldDefinition])

  private object ComplexType {

    def fromDefinition(td: TypeDefinition): OrMissing[ComplexType] =
      td match {
        case o: ObjectTypeDefinition => Right(ComplexType(o.name, o.values))
        case o: InterfaceTypeDefinition => Right(ComplexType(o.name, o.values))
        case _ => Left(TypeError.Todo(s"fromDefinition(${td.name})"))
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
    * Try to dig the given type out of the schema,
    * we ignore any type modifiers and just search for a type with the same name
    * but we include the input type including modifiers in the response
    */
  private def findTypeDefinition(schema: Schema, tpe: Type): OrMissing[TypeDefinition] =
    (predefinedTypes ++ schema).find(_.name == typeName(tpe))
      .toRight(MissingType(typeName(tpe).value))

  /**
    * Given a complex type and a field name, find the definition of the field.
    * if the complex type doesn't have that field then an error will be raised
    */
  def findFieldDefinition(ct: ComplexType, fieldName: FieldName): Either[TypeError, FieldDefinition] =
    ct.fields.find(_.name.value == fieldName.value).toRight(TypeError.Todo(s"findDirect(${ct.name}, $fieldName)"))

  /**
    * Exists purely to match fields on whether
    * they're recursive (have a SelectionSet) or not
    */
  private object Node {
    def unapply(f: Field): Option[SelectionSet] = f.selectionSet
  }

  /**
    * Given a cursor and the type for a field
    * then create a TypeTree field with the given inlined type
    */
  private def createField(c: Cursor)(tpe: TypeTree): TypeTree.Field =
    TypeTree.Field(c.focus.name, tpe, fromType( c.parent.fold(c.focus.definition)(_.definition).`type`))

  /**
    * Create a field from a cursor, assuming the cursor is pointing
    * at a scalar type definition. Will fail with a TypeError if it isn't
    */
  private def createScalarField(cursor: Cursor): Either[TypeError, TypeTree.Field] =
    createFromScalar(cursor).map(createField(cursor))

  /**
    * Given a recursive SelectionSet, go down through it
    * and create a TypeTree by inlining all the type definitions of it's fields
    */
  private def createTree(cursor: Cursor, selectionSet: SelectionSet): Either[TypeError, TypeTree] =
    selectionSet.fields.traverse {
      case f @ Node(other) => cursor.down(f).flatMap(c => createTree(c, other).map(createField(c)))
      case f               => cursor.down(f).flatMap(c => createScalarField(c))
    }.map(fields => TypeTree.Object(cursor.focus.tpe.name.value, fields))

  /**
    * Perform the type checking, that is go through all the fields in the query,
    * find out what their type should be and inline that type into a TypeTree
    */
  def run(schema: Schema, operationDefinition: OperationDefinition): OrMissing[TypeTree] =
    for {
      root <- findTypeDefinition(schema, NamedType(Name("Query")))
      queryDef = FieldDefinition(None, Name("Query"), List.empty, NamedType(Name("Query")), List.empty)
      cursor = Cursor(schema, Vector.empty, CursorField(FieldName(None, "Query"), queryDef, root))
      result <- createTree(cursor, operationDefinition.selectionSet)
    } yield result
}
