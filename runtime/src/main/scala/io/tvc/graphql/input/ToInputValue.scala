package io.tvc.graphql.input

import enumeratum.EnumEntry
import higherkindness.droste.data.Fix
import io.tvc.graphql.input.InputValue._
import magnolia._

import scala.language.experimental.macros

trait ToInputValue[A] {
  def to(a: A): RecInputValue
}

object ToInputValue {

  type Typeclass[T] = ToInputValue[T]

  implicit val stringToInputValue: ToInputValue[String] =
    str => InputValue.string(str)

  implicit val intToInputValue: ToInputValue[Boolean] =
    str => Fix[InputValue](BooleanInputValue(str))

  implicit def enumToInputValue[A <: EnumEntry]: ToInputValue[A] =
    enum => InputValue.enum(enum.entryName)

  implicit def listToInputValue[A: ToInputValue]: ToInputValue[List[A]] =
    list => InputValue.list(list.map(implicitly[ToInputValue[A]].to):_*)

  def combine[T](ctx: CaseClass[ToInputValue, T]): ToInputValue[T] =
    (v: T) =>
      Fix(
        InputValue.ObjectInputValue[RecInputValue](
          ctx.parameters.map(p => ObjectField(p.label, p.typeclass.to(p.dereference(v)))).toList
        )
      )

  def derive[T]: ToInputValue[T] =
    macro Magnolia.gen[T]
}
