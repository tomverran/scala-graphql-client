package io.tvc.graphql.input

import org.scalacheck.{Gen, Prop}
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.Checkers

class InputValueTest extends WordSpec with Matchers with Checkers {

  "InputValue.serialise" should {

    "Serialise alphanumeric strings as-is" in {
      check(
        Prop.forAll(Gen.alphaNumStr) { str =>
          InputValue.serialise(InputValue.string(str)) == s""""${str}""""
        }
      )
    }

    "Escape quote characters in strings" in {
      pending
    }
  }

}
