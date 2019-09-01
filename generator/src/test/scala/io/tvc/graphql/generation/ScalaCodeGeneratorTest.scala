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
        """.trim.stripMargin
    }

    "Produce correct query signatures given no variables" in {
      val qry = Query("Foo", "{}", None)
      queryFunction(qry, TypeRef("Result")) shouldBe
        s"""
           |def apply: Request[Response[Result]] =
           |  Request[Response[Result]](
           |    query = $quotes
           |    |{}
           |    $quotes.trim.stripMargin
           |  )
         """.trim.stripMargin
    }

    "Produce correct query signatures given a variable" in {

      val va = Field(FieldName(None, "foo"), InputValue(default = None, TypeRef("Baz")), List(NonNullType))
      val qry = Query("Foo", "{foo(bar:$baz)}", Some(TypeTree.Object(Metadata(None, "Variables"), List(va))))
      queryFunction(qry, TypeRef("Result")) shouldBe
      s"""
        |def apply(foo: Baz): Request[Response[Result]] =
        |  Request[Response[Result]](
        |    query = $quotes
        |    |{foo(bar:$$baz)}
        |    $quotes.trim.stripMargin,
        |    variables = Variables(foo).asJson
        |  )
      """.trim.stripMargin
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
