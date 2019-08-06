package io.tvc.graphql.inlining

import io.tvc.graphql.parsing.CommonModel.{Name, Type}
import io.tvc.graphql.parsing.CommonModel.Type.{ListType, NamedType, NonNullType}
import io.tvc.graphql.parsing.SchemaModel.{Schema, TypeDefinition}
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition.ScalarTypeDefinition
import io.tvc.graphql.inlining.Utilities.TypeError.{MissingType, OrMissing}
import io.tvc.graphql.inlining.TypeTree.TypeModifier
import io.tvc.graphql.inlining.TypeTree.TypeModifier.NullableType

import scala.annotation.tailrec

object Utilities {

  sealed trait TypeError

  object TypeError {
    type OrMissing[A] = Either[TypeError, A]
    case class MissingType(name: String) extends TypeError
    case class MissingField(name: String) extends TypeError
    case class ExpectedFields(field: String) extends TypeError
    case class Todo(name: String) extends TypeError
  }

   /**
    * https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
    */
  val predefinedTypes: List[TypeDefinition] =
    List("Int", "Float", "Boolean", "String", "ID").map(t => ScalarTypeDefinition(None, Name(t), List.empty))

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
  def findTypeDefinition(schema: Schema, tpe: Type): OrMissing[TypeDefinition] =
    (predefinedTypes ++ schema).find(_.name == typeName(tpe))
      .toRight(MissingType(typeName(tpe).value))

  /**
    * Given a type, dig through it to find out what the modifiers are
    * They should then be applied left-to-right to the eventual type name
    */
  def modifiers(tpe: Type, mods: Vector[TypeModifier] = Vector.empty): Vector[TypeModifier] =
    tpe match {
      case Type.NonNullType(t) => modifiers(t, mods :+ TypeModifier.NonNullType)
      case a if !mods.lastOption.contains(TypeModifier.NonNullType) &&
                !mods.lastOption.contains(TypeModifier.NullableType) => modifiers(a, mods :+ NullableType)
      case Type.ListType(a) => modifiers(a, mods :+ TypeModifier.ListType)
      case Type.NamedType(_) => mods
    }
}
