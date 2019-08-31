package io.tvc.graphql.input

import cats.Contravariant
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

  def apply[A: ToInputValue]: ToInputValue[A] =
    implicitly

  implicit val contravariant: Contravariant[ToInputValue] = new Contravariant[Typeclass] {
    def contramap[A, B](fa: ToInputValue[A])(f: B => A): ToInputValue[B] = b => fa.to(f(b))
  }

  implicit val stringToInputValue: ToInputValue[String] = InputValue.string
  implicit val booleanToInputValue: ToInputValue[Boolean] = InputValue.boolean
  implicit val floatToInputValue: ToInputValue[Float] = InputValue.float
  implicit val intToInputValue: ToInputValue[Int] = InputValue.integer

  implicit def optionToInputValue[A: ToInputValue]: ToInputValue[Option[A]] =
    _.fold[RecInputValue](Fix[InputValue](NullInputValue))(ToInputValue[A].to)

  implicit def enumToInputValue[A <: EnumEntry]: ToInputValue[A] =
    enum => InputValue.enum(enum.entryName)

  implicit def listToInputValue[A: ToInputValue]: ToInputValue[List[A]] =
    list => InputValue.list(list.map(ToInputValue[A].to):_*)

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
