package io.tvc.graphql.generation

import io.tvc.graphql.generation.ScalaCodeGenerator._
import io.tvc.graphql.generation.TypeDeduplicator.TypeRef
import io.tvc.graphql.inlining.InputInliner.InputValue
import io.tvc.graphql.inlining.TypeTree
import io.tvc.graphql.inlining.TypeTree.TypeModifier.NonNullType
import io.tvc.graphql.inlining.TypeTree.{Field, FieldName, Metadata, Scalar}
import org.scalatest.{Matchers, WordSpec}

class ScalaCodeGeneratorTest extends WordSpec with Matchers {

  val quotes: String = "\"\"\""

  "Scala code generator" should {

    "Produce correct looking case classes" in {
      val fieldTypeRef: Either[TypeRef, InputValue[TypeRef]] = Left(TypeRef("Baz"))
      val field = Field(FieldName(None, "foo"), fieldTypeRef, List(NonNullType))
      val obj = TypeTree.Object(Metadata(None, "Inputs"), List(field))
      caseClass(obj) shouldBe
        """
          |@JsonCodec
          |case class Inputs(foo: Baz)
          |
          |object Inputs {
          |  implicit val toInput: ToInputValue[Inputs] = ToInputValue.derive
          |}
        """.trim.stripMargin
    }

    "Produce correct query signatures given no variables" in {
      val qry = Query("Foo", "Query", "{}", TypeTree.Object(Metadata(None, "Inputs"), List.empty))
      queryFunction(qry) shouldBe s"""
        |def apply(): String =
        |  $quotes
        |  |query Foo()
        |  |{}
        |  $quotes.stripMargin""".stripMargin.trim
    }

    "Produce correct query signatures given a variable" in {

      val va = Field(FieldName(None, "foo"), InputValue(default = None, TypeRef("Baz")), List(NonNullType))
      val qry = Query("Foo", "Query", "{}", TypeTree.Object(Metadata(None, "Inputs"), List(va)))
      queryFunction(qry) shouldBe s"""
        |def apply(foo: Baz): String =
        |  $quotes
        |  |query Foo($$foo:$${Baz.toInput.to(foo)})
        |  |{}
        |  $quotes.stripMargin""".stripMargin.trim
    }

    "Not generate case classes for built in types" in {
      scalaCode(Scalar(Metadata(None, "Float"))) shouldBe ""
      scalaCode(Scalar(Metadata(None, "Boolean"))) shouldBe ""
      scalaCode(Scalar(Metadata(None, "String"))) shouldBe ""
      scalaCode(Scalar(Metadata(None, "Int"))) shouldBe ""
      scalaCode(Scalar(Metadata(None, "ID"))) shouldBe ""
    }
  }
}
