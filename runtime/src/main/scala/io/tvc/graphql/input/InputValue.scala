package io.tvc.graphql.input

import cats.Traverse
import cats.derived.semi
import cats.syntax.functor._
import higherkindness.droste.Algebra
import higherkindness.droste.data.Fix
import higherkindness.droste._

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
  case class IntegerInputValue(value: Int) extends InputValue[Nothing]
  case class FloatInputValue(value: Float) extends InputValue[Nothing]

  case class ObjectField[A](name: String, value: A)
  case class ObjectInputValue[A](values: List[ObjectField[A]]) extends InputValue[A]
  case class ListInputValue[A](values: List[A]) extends InputValue[A]

  implicit val fieldTraverse: Traverse[ObjectField] =
   semi.traverse[ObjectField]

  implicit val inputValueTraverse: Traverse[InputValue] =
    semi.traverse[InputValue]

  def obj(fields: (String, RecInputValue)*): RecInputValue =
    Fix[InputValue](ObjectInputValue(fields.map { case (n, v) => ObjectField(n, v) }.toList))

  def string(str: String): RecInputValue =
    Fix[InputValue](StringInputValue(str))

  def list(list: RecInputValue*): RecInputValue =
    Fix[InputValue](ListInputValue(list.toList))

  def boolean(bool: Boolean): RecInputValue =
    Fix[InputValue](BooleanInputValue(bool))

  def integer(int: Int): RecInputValue =
    Fix[InputValue](IntegerInputValue(int))

  def float(float: Float): RecInputValue =
    Fix[InputValue](FloatInputValue(float))

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
      case e => e.toInt.formatted("\\u%04x")
    }.mkString

  def serialise(iv: RecInputValue): String =
    scheme.cata(
      Algebra[InputValue, String] {
        case EnumInputValue(v) => v
        case FloatInputValue(v) => v.toString
        case IntegerInputValue(v) => v.toString
        case BooleanInputValue(v) => v.toString
        case StringInputValue(v) => s""""${escape(v)}""""
        case ListInputValue(vs) => s"[${vs.mkString(" ")}]"
        case ObjectInputValue(vs) => s"{${vs.map(v => s"${v.name}:${v.value}").mkString(" ")}}"
      }
    ).apply(iv)
}
