package io.tvc.graphql.inlining

import cats.Traverse
import cats.derived.semi
import io.tvc.graphql.Fix
import io.tvc.graphql.inlining.TypeTree.Metadata

import scala.language.higherKinds

sealed trait TypeTree[+A] {
  def meta: Metadata
}

object TypeTree {

  case class Metadata(
    comment: Option[String],
    name: String,
  )

  case class Scalar(meta: Metadata) extends TypeTree[Nothing]
  case class Union[A](meta: Metadata, fields: List[A]) extends TypeTree[A]
  case class Enum(meta: Metadata, fields: List[String]) extends TypeTree[Nothing]
  case class Object[A](meta: Metadata, fields: List[Field[A]]) extends TypeTree[A]
  case class Field[A](name: FieldName, `type`: A, modifiers: List[TypeModifier])

  /**
    * The name of a field in the output AST,
    * we include alias information for deduplication
    */
  case class FieldName(alias: Option[String], value: String) {
    override def toString: String = (alias.toList :+ value).mkString(":")
  }

  implicit val fieldTraverse: Traverse[Field] =
    semi.traverse[Field]

  /**
    * A modifier for a type,
    * i.e. the type is a list type etc.
    * Modifiers should be applied recursively in order
    */
  sealed trait TypeModifier

  object TypeModifier {
    case object ListType extends TypeModifier
    case object NullableType extends TypeModifier
    case object NonNullType extends TypeModifier
  }

  type RecTypeTree = Fix[TypeTree]

  def scalar(name: Metadata): RecTypeTree =
    Fix[TypeTree](Scalar(name))

  def enum(name: Metadata, fields: List[String]): RecTypeTree =
    Fix[TypeTree](Enum(name, fields))

  def obj(name: Metadata, fields: List[Field[RecTypeTree]]): RecTypeTree =
    Fix[TypeTree](Object(name, fields))

  implicit val objTraverse: Traverse[Object] =
    semi.traverse[Object]

  implicit val ttTraverse: Traverse[TypeTree] =
    semi.traverse[TypeTree]
}

