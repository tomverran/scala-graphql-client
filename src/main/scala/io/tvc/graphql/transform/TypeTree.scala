package io.tvc.graphql.transform

import io.tvc.graphql.transform.TypeChecker.{FieldName, TypeModifier}

import scala.language.higherKinds

sealed trait TypeTree[+A]

object TypeTree {

  case class Scalar(name: String) extends TypeTree[Nothing]
  case class Union[A](name: String, fields: List[A]) extends TypeTree[A]
  case class Enum(name: String, fields: List[String]) extends TypeTree[Nothing]
  case class Object[A](name: String, fields: List[Field[A]]) extends TypeTree[A]
  case class Field[A](name: FieldName, `type`: A, modifiers: List[TypeModifier])

  case class Fix[F[_]](unfix: F[Fix[F]])
  type RecTypeTree = Fix[TypeTree]

  def scalar(name: String): RecTypeTree =
    Fix[TypeTree](Scalar(name))

  def enum(name: String, fields: List[String]): RecTypeTree =
    Fix[TypeTree](Enum(name, fields))

  def obj(name: String, fields: List[Field[RecTypeTree]]): RecTypeTree =
    Fix[TypeTree](Object(name, fields))
}

