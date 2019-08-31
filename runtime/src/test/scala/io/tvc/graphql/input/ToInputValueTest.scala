package io.tvc.graphql.input

import enumeratum._
import io.tvc.graphql.input.InputValue._
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.IndexedSeq
import scala.language.higherKinds

class ToInputValueTest extends WordSpec with Matchers {

  case class Empty()
  case class Stringy(first: String, second: String)
  sealed trait Ship extends EnumEntry

  object Ship extends Enum[Ship] {
    val values: IndexedSeq[Ship] = findValues
    case object Caravel extends Ship
    case object Sloop extends Ship
  }

  case class Navy(ships: List[Ship])

  "ToInputValue derivation" should {

    "Work for an empty case class" in {
      val expected = InputValue.obj("Empty")
      ToInputValue.derive[Empty].to(Empty()) shouldBe expected
    }

    "Work for a case class of strings" in {
      val expected = InputValue.obj("Stringy", "first" -> string("foo"), "second" -> string("bar"))
      ToInputValue.derive[Stringy].to(Stringy("foo", "bar")) shouldBe expected
    }


    "Work for a case class containing an Enumeratum enum" in {
      val exp = InputValue.obj("Navy", "ships" -> list(enum("Caravel"), enum("Sloop")))
      ToInputValue.derive[Navy].to(Navy(List(Ship.Caravel, Ship.Sloop))) shouldBe exp
    }
  }
}
