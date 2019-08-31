package io.tvc.graphql.input

import enumeratum._
import higherkindness.droste.data.Fix
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
      val expected = InputValue.obj()
      ToInputValue.derive[Empty].to(Empty()) shouldBe expected
    }

    "Work for a case class of strings" in {
      val expected = InputValue.obj("first" -> string("foo"), "second" -> string("bar"))
      ToInputValue.derive[Stringy].to(Stringy("foo", "bar")) shouldBe expected
    }


    "Work for a case class containing an Enumeratum enum" in {
      val exp = InputValue.obj("ships" -> list(enum("Caravel"), enum("Sloop")))
      ToInputValue.derive[Navy].to(Navy(List(Ship.Caravel, Ship.Sloop))) shouldBe exp
    }
  }

  "ToInputValue instances" should {

    "Return NullInputValue for options" in {
      ToInputValue[Option[String]].to(None) shouldBe Fix[InputValue](NullInputValue)
      ToInputValue[Option[String]].to(Some("foo")) shouldBe string("foo")
    }

    "Work for scalar types" in {
      ToInputValue[String].to("hi") shouldBe string("hi")
      ToInputValue[Float].to(123.4f) shouldBe float(123.4f)
      ToInputValue[Int].to(2) shouldBe integer(2)
    }

  }
}
