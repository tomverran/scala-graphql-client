package io.tvc.graphql.inlining

import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Eval, Traverse}
import io.tvc.graphql.recursion.Fix
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

  /**
    * This brutal traverse instance is used to be able to
    * deal with the above recursive tree structure (i.e. nested in a Fix)
    */
  implicit val ttTraverse: Traverse[TypeTree] = new Traverse[TypeTree] {
    def traverse[G[_], A, B](fa: TypeTree[A])(f: A => G[B])(implicit G: Applicative[G]): G[TypeTree[B]] =
      fa match {
        case e: Enum => G.pure(e)
        case s: Scalar => G.pure(s)
        case u: Union[A] =>
          u.fields.traverse(f)
            .map(fs => u.copy(fields = fs))
        case o: Object[A] =>
          o.fields.traverse(fld => f(fld.`type`)
            .map(r => fld.copy(`type` = r)))
            .map(fs => o.copy(fields = fs))
      }

    def foldLeft[A, B](fa: TypeTree[A], b: B)(f: (B, A) => B): B =
      fa match {
        case _: Enum => b
        case _: Scalar => b
        case u: Union[A] => u.fields.foldLeft(b)(f)
        case o: Object[A] => o.fields.foldLeft(b) { case (bb, fld) => f(bb, fld.`type`) }
      }

    def foldRight[A, B](fa: TypeTree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case _: Enum => lb
        case _: Scalar => lb
        case u: Union[A] => u.fields.foldr(lb)(f)
        case o: Object[A] => o.fields.foldr(lb) { case (fld, bb) => f(fld.`type`, bb) }
      }
  }
}

