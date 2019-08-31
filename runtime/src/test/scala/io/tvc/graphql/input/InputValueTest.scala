package io.tvc.graphql.input

import io.tvc.graphql.input.InputValue._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.Checkers

class InputValueTest extends WordSpec with Matchers with Checkers {
  val quote = "\""

  val inputValue: Gen[RecInputValue] =
    Gen.oneOf(
      Arbitrary.arbInt.arbitrary.map(integer),
      Arbitrary.arbFloat.arbitrary.map(float),
      Arbitrary.arbBool.arbitrary.map(boolean),
    )

  "InputValue.serialise: strings" should {

    "Serialise alphanumeric strings as-is" in {
      check(
        Prop.forAll(Gen.alphaNumStr) { str =>
          serialise(string(str)) == s"$quote$str$quote"
        }
      )
    }

    "Escape quote characters in strings" in {
      check(
        Prop.forAll(Gen.alphaNumStr) { str =>
          serialise(string(serialise(string(str)))) == s"$quote\\$quote$str\\$quote$quote"
        }
      )
    }

    "Escape non ascii unicode characters" in {
      serialise(string("\u0001")) shouldBe s"$quote\\u0001$quote"
    }
  }

  "InputValue.serialise: other scalars" should {

    "Serialise bools to lowerase true/false literals" in {
      serialise(boolean(false)) shouldBe "false"
      serialise(boolean(true)) shouldBe "true"
    }

    "Serialise all integers" in {
      check(
        Prop.forAll(Arbitrary.arbInt.arbitrary) { int =>
          serialise(integer(int)).matches("^-?0$|^-?[1-9]+[0-9]*$")
        }
      )
    }

    "Serialise all floats" in {
      check(
        Prop.forAll(Arbitrary.arbFloat.arbitrary) { f =>
          serialise(float(f)).matches("-?\\d+(\\.\\d+)?([Ee]-?\\d+)?")
        }
      )
    }

    "serialise enums as bare strings" in {
      check(
        Prop.forAll(Arbitrary.arbString.arbitrary) { e =>
          serialise(enum(e)) == e
        }
      )
    }
  }

  "InputValue.serialise: Lists" should {

    "serialise an empty list" in {
      serialise(list()) shouldBe "[]"
    }

    "Use spaces to separate elems since commas are optional" in {
      check(
        Prop.forAll(Gen.listOf(inputValue)) { ivs =>
          serialise(list(ivs:_*)) == s"[${ivs.map(serialise).mkString(" ")}]"
        }
      )
    }
  }

  "InputValue.serialise: Objects" should {

    "serialise an empty object" in {
      serialise(obj()) shouldBe "{}"
    }

    "Use spaces to separate elems since commas are optional" in {
      check(
        Prop.forAll(Gen.listOf(Gen.zip(Gen.alphaNumStr, inputValue))) { ivs =>
          val expected = ivs.map { case (k, v) => s"$k:${serialise(v)}"}.mkString(" ")
          serialise(obj(ivs:_*)) == s"{$expected}"
        }
      )
    }
  }
}
