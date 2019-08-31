package io.tvc.graphql.generation

import io.tvc.graphql.generation.ScalaCodeGenerator.query
import io.tvc.graphql.generation.TypeDeduplicator.TypeRef
import io.tvc.graphql.inlining.InputInliner.InputValue
import io.tvc.graphql.inlining.TypeTree
import io.tvc.graphql.inlining.TypeTree.TypeModifier.NonNullType
import io.tvc.graphql.inlining.TypeTree.{Field, FieldName, Metadata}
import org.scalatest.{Matchers, WordSpec}

class ScalaCodeGeneratorTest extends WordSpec with Matchers {

  val quotes: String = "\"\"\""

  "Scala code generator" should {

    "Produce correct query signatures given no variables" in {
      val result = query("Foo", TypeTree.Object(Metadata(None, "Inputs"), List.empty), "{}")
      result shouldBe s"""
        |def Foo(): String =
        |  $quotes
        |  |{}
        |  $quotes.stripMargin""".stripMargin.trim
    }

    "Produce correct query signatures given a variable" in {

      val va = Field(FieldName(None, "foo"), InputValue(default = None, TypeRef("Baz")), List(NonNullType))
      val result = query("Foo", TypeTree.Object(Metadata(None, "Inputs"), List(va)), "{}")
      result shouldBe s"""
        |def Foo(foo: Baz): String =
        |  $quotes
        |  |{}
        |  $quotes.stripMargin""".stripMargin.trim
    }
  }
}
