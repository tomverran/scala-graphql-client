package io.tvc.graphql.input

import cats.Traverse
import cats.derived.semi
import cats.syntax.functor._
import io.tvc.graphql.Fix

import scala.language.higherKinds

/**
  * This is broadly similar to the TypeTree used in generating code
  * but it represents values rather than types. We need this to be able to serialise
  * the classes we generate back into values GraphQL understands
  */
sealed trait InputValue[+A]

object InputValue {

  type RecInputValue = Fix[InputValue]

  case class EnumInputValue(value: String) extends InputValue[Nothing]
  case class BooleanInputValue(value: Boolean) extends InputValue[Nothing]
  case class StringInputValue(value: String) extends InputValue[Nothing]

  case class ObjectField[A](name: String, value: A)
  case class ObjectInputValue[A](name: String, values: List[ObjectField[A]]) extends InputValue[A]
  case class ListInputValue[A](values: List[A]) extends InputValue[A]

  implicit val fieldTraverse: Traverse[ObjectField] =
   semi.traverse[ObjectField]

  implicit val inputValueTraverse: Traverse[InputValue] =
    semi.traverse[InputValue]

  def obj(name: String, fields: (String, RecInputValue)*): RecInputValue =
    Fix[InputValue](ObjectInputValue(name, fields.map { case (n, v) => ObjectField(n, v) }.toList))

  def string(str: String): RecInputValue =
    Fix[InputValue](StringInputValue(str))

  def list(list: RecInputValue*): RecInputValue =
    Fix[InputValue](ListInputValue(list.toList))

  def boolean(bool: Boolean): RecInputValue =
    Fix[InputValue](BooleanInputValue(bool))

  def enum(value: String): RecInputValue =
    Fix[InputValue](EnumInputValue(value))

  private object SourceText {

    def unapply(c: Char): Option[String] =
      if (
        c == '\u0009' || c == '\u000A' || c == '\u000D' ||
        (c >= '\u0020' && c <= '\uFFFF')
      ) Some(c.toString) else None
  }

  private def escape(str: String): String =
    str.toCharArray.map {
      case '"' => "\\\""
      case SourceText(c) => c
      case e => s"\\u${e.formatted("%04x")}"
    }.mkString

  def serialise(a: RecInputValue): String =
    Fix.fold[InputValue, String](a) {
      case EnumInputValue(v) => v
      case BooleanInputValue(v) => v.toString
      case StringInputValue(v) => s""""${escape(v)}""""
      case ListInputValue(vs) => s"[${vs.mkString(",")}]"
      case ObjectInputValue(_, vs) => s"{ ${vs.map(v => s"${v.name}: ${v.value}").mkString(", ")}"
    }
}
