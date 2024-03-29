package io.tvc.graphql.inlining

import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import higherkindness.droste.data.Fix
import io.tvc.graphql.inlining.TypeTree.{FieldName, Metadata, Object, RecTypeTree}
import io.tvc.graphql.inlining.Utilities.TypeError._
import io.tvc.graphql.inlining.Utilities._
import io.tvc.graphql.parsing.CommonModel.Type.NamedType
import io.tvc.graphql.parsing.CommonModel.{Name, OperationType}
import io.tvc.graphql.parsing.QueryModel.{Field, SelectionSet}
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition._
import io.tvc.graphql.parsing.SchemaModel.{FieldDefinition, Schema, TypeDefinition}

/**
  * Here we take a query and a schema and produce a TypeTree AST.
  * This is a recursive structure of complex types returned by the query
  */
object OutputInliner {

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
    * Given a cursor try to turn it into a Scalar type tree by matching on it's current definition
    * Will fail if the definition isn't an enum or a scalar type
    */
  def createScalar(tpe: TypeDefinition): OrMissing[TypeTree[Nothing]] =
    tpe match {
      case e: EnumTypeDefinition =>
        Right(
          TypeTree.Enum(
            Metadata(e.description.map(_.value), e.name.value),
            e.values.map(_.value.value.value)
          )
        )
      case s: ScalarTypeDefinition =>
        Right(TypeTree.Scalar(Metadata(s.description.map(_.value), s.name.value)))
      case _ =>
        Left(ExpectedFields(tpe.name.value))
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
  private def createField(c: Cursor)(tpe: RecTypeTree): TypeTree.Field[RecTypeTree] =
    TypeTree.Field(
      `type` = tpe,
      name = c.focus.name,
      modifiers = modifiers(c.focus.definition.`type`).toList
    )

  /**
    * Create a field from a cursor, assuming the cursor is pointing
    * at a scalar type definition. Will fail with a TypeError if it isn't
    */
  private def createScalarField(cursor: Cursor): Either[TypeError, TypeTree.Field[RecTypeTree]] =
    createScalar(cursor.focus.tpe).map(f => createField(cursor)(Fix[TypeTree](f)))

  /**
    * Given a recursive SelectionSet, go down through it
    * and create a TypeTree by inlining all the type definitions of it's fields
    */
  private def createTree(cursor: Cursor, selectionSet: SelectionSet): OrMissing[Object[RecTypeTree]] =
    selectionSet.fields.traverse {
      case f @ Node(other) =>
        cursor.down(f).flatMap(c => createTree(c, other).map(f => createField(c)(Fix[TypeTree](f))))
      case f               =>
        cursor.down(f).flatMap(c => createScalarField(c))
    }.map { fields =>
      Object(
        Metadata(cursor.focus.tpe.description.map(_.value), cursor.focus.tpe.name.value),
        fields
      )
    }

  /**
    * From the GraphQL spec:
    * While any type can be the root operation type for a GraphQL operation, the type system definition
    * language can omit the schema definition when the query, mutation, and subscription root types are named
    * Query, Mutation, and Subscription respectively.
    */
  private def findRoot(schema: Schema, op: OperationType): Name =
    schema.schemaDefinition
      .flatMap(_.definitions.find(_.operationType == op))
      .fold(Name(op.toString))(_.name)

  /**
    * Perform the type checking, that is go through all the fields in the query,
    * find out what their type should be and inline that type into a TypeTree
    */
  def run(schema: Schema, op: OperationType, select: SelectionSet): OrMissing[Object[RecTypeTree]] = {
    val rootName = findRoot(schema, op)
    for {
      root <- findTypeDefinition(schema, NamedType(rootName))
      queryDef = FieldDefinition(None, rootName, List.empty, NamedType(rootName), List.empty)
      cursor = Cursor(schema, Vector.empty, CursorField(FieldName(None, rootName.value), queryDef, root))
      result <- createTree(cursor, select)
    } yield result
  }
}
