package io.tvc.graphql.parsing

import cats.data.NonEmptyList
import io.tvc.graphql.parsing.CommonModel.OperationType.{Mutation, Query}
import io.tvc.graphql.parsing.CommonModel.Type.{ListType, NamedType, NonNullType}
import io.tvc.graphql.parsing.CommonModel.Value.{EnumValue, IntValue, StringValue}
import io.tvc.graphql.parsing.CommonModel.{Argument, Directive, Name}
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition._
import io.tvc.graphql.parsing.SchemaModel._
import org.scalatest.{Matchers, WordSpec}

/**
  * Rudimentary checks for schema parsing
  * Hand writing test cases like this is not much fun,
  * this is really to be added to if bugs are discovered
  */
class SchemaParserTest extends WordSpec with Matchers {

  // these pesky quotes :(
  val quotes: String = "\"\"\""

  "Schema parser: Scalars" should {

    "Parse a very basic scalar type" in {
      val result = SchemaParser.parse("scalar foo")
      result shouldBe Right(
        Schema(
          schemaDefinition = None,
          typeDefinitions = Map(Name("foo") -> ScalarTypeDefinition(None, Name("foo"), List.empty))
        )
      )
    }

    "Parse a more complex scalar type" in {
      val result = SchemaParser.parse(""""foo scalar" scalar foo @deprecated(why:"Just is") @ok()""")
      result shouldBe Right(
        Schema(
          schemaDefinition = None,
          typeDefinitions = Map(
            Name("foo") -> ScalarTypeDefinition(
              description = Some(Description("foo scalar")),
              name = Name("foo"),
              directives = List(
                Directive(Name("deprecated"), List(Argument(Name("why"), StringValue("Just is")))),
                Directive(Name("ok"), List.empty)
              )
            )
          )
        )
      )
    }
  }

  "Schema parser: Comments" should {

    "Ignore anything after a hash until a line end" in {
      val result = SchemaParser.parse("#scalar bar\nscalar foo")
      result shouldBe Right(
        Schema(
          schemaDefinition = None,
          typeDefinitions = Map(Name("foo") -> ScalarTypeDefinition(None, Name("foo"), List.empty))
        )
      )
    }
  }

  "Schema parser: Enums" should {

    "Parse an empty enum" in {
      // this isn't actually a valid GraphQL enum but I figure the parser doesn't care
      val expected = EnumTypeDefinition(None, Name("blah"), List.empty, List.empty)
      SchemaParser.parse("enum blah {}") shouldBe Right(Schema(None, Map(Name("blah") -> expected)))
    }

    "Parse a relatively complex enum" in {

      val input =
        s"""
          |$quotes
          |Birds are feathery creatures prone to flight
          |$quotes
          |enum Bird @group(class:"Aves") {
          |  MAGPIE
          |  DODO @extinct(since: "1681"),
          |  CROW,
          |  Robin,
          |  $quotes
          |  Clever
          |  $quotes
          |  RAVEN
          |}
        """.stripMargin

      SchemaParser.parse(input.trim) shouldBe Right(
        Schema(
          schemaDefinition = None,
          typeDefinitions = Map(
            Name("Bird") -> EnumTypeDefinition(
              Some(Description("Birds are feathery creatures prone to flight")),
              Name("Bird"),
              List(Directive(Name("group"), List(Argument(Name("class"), StringValue("Aves"))))),
              List(
                EnumValueDefinition(None, EnumValue(Name("MAGPIE")), List.empty),
                EnumValueDefinition(None, EnumValue(Name("DODO")), List(
                  Directive(Name("extinct"), List(Argument(Name("since"), StringValue("1681"))))
                )),
                EnumValueDefinition(None, EnumValue(Name("CROW")), List.empty),
                EnumValueDefinition(None, EnumValue(Name("Robin")), List.empty),
                EnumValueDefinition(Some(Description("Clever")), EnumValue(Name("RAVEN")), List.empty),
              )
            )
          )
        )
      )
    }
  }

  "Schema parser: Objects" should {

    "Parse an empty object" in {
      val expected = ObjectTypeDefinition(None, Name("blah"), List.empty, List.empty, List.empty)
      SchemaParser.parse("type blah {}") shouldBe Right(Schema(None, Map(Name("blah") -> expected)))
    }

    "Parse a complex object" in {

      val input =
        s"""
           |$quotes
           |Creates abstract proxies
           |$quotes
           |type AbstractProxyFactory implements Factory & Proxy @since(version: 3) {
           |  name: String
           |  $quotes
           |   The proxies
           |  $quotes
           |  proxies(abstractivity: Int! = 3 @hi): [Proxy!]! @foo
           |}
        """.stripMargin

      SchemaParser.parse(input.trim) shouldBe Right(
        Schema(
          schemaDefinition = None,
          typeDefinitions = Map(
            Name("AbstractProxyFactory") -> ObjectTypeDefinition(
              description = Some(Description("Creates abstract proxies")),
              name = Name("AbstractProxyFactory"),
              implements = List(Name("Factory"), Name("Proxy")),
              directives = List(Directive(Name("since"), List(Argument(Name("version"), IntValue(3))))),
              values = List(
                FieldDefinition(None, Name("name"), List.empty, NamedType(Name("String")), List.empty),
                FieldDefinition(
                  description = Some(Description("The proxies")),
                  name = Name("proxies"),
                  arguments = List(
                    InputValueDefinition(
                      description = None,
                      name = Name("abstractivity"),
                      `type` = NonNullType(NamedType(Name("Int"))),
                      defaultValue = Some(IntValue(3)),
                      directives = List(Directive(Name("hi"), List.empty))
                    )
                  ),
                  `type` = NonNullType(ListType(NonNullType(NamedType(Name("Proxy"))))),
                  directives = List(Directive(Name("foo"), List.empty))
                )
              )
            )
          )
        )
      )
    }
  }

  "Schema parser: Interfaces" should {

    "Parse an empty interface" in {
      val expected = InterfaceTypeDefinition(None, Name("blah"), List.empty, List.empty)
      SchemaParser.parse("interface blah {}") shouldBe Right(Schema(None, Map(Name("blah") -> expected)))
    }
    "Parse a complex interface" in {

      val input =
        s"""
           |$quotes
           |Can you tell I copy pasted this
           |$quotes
           |interface AbstractProxyFactory @since(version: 3) {
           |  name: String
           |  $quotes
           |   The proxies
           |  $quotes
           |  proxies(abstractivity: Int! = 3 @hi): [Proxy!]! @foo
           |}
        """.stripMargin

      SchemaParser.parse(input.trim) shouldBe Right(
        Schema(
          schemaDefinition = None,
          typeDefinitions = Map(
            Name("AbstractProxyFactory") -> InterfaceTypeDefinition(
              description = Some(Description("Can you tell I copy pasted this")),
              name = Name("AbstractProxyFactory"),
              directives = List(Directive(Name("since"), List(Argument(Name("version"), IntValue(3))))),
              values = List(
                FieldDefinition(None, Name("name"), List.empty, NamedType(Name("String")), List.empty),
                FieldDefinition(
                  description = Some(Description("The proxies")),
                  name = Name("proxies"),
                  arguments = List(
                    InputValueDefinition(
                      description = None,
                      name = Name("abstractivity"),
                      `type` = NonNullType(NamedType(Name("Int"))),
                      defaultValue = Some(IntValue(3)),
                      directives = List(Directive(Name("hi"), List.empty))
                    )
                  ),
                  `type` = NonNullType(ListType(NonNullType(NamedType(Name("Proxy"))))),
                  directives = List(Directive(Name("foo"), List.empty))
                )
              )
            )
          )
        )
      )
    }
  }

  "Schema parser: Unions" should {

    "Parse an empty union" in {
      // again this isn't valid GraphQL but I am not the GraphQL police
      val expected = UnionTypeDefinition(None, Name("blah"), List.empty, List.empty)
      SchemaParser.parse("union blah = ") shouldBe Right(Schema(None, Map(Name("blah") -> expected)))
    }

    "Parse a more complex union" in {

      val input =
        s"""
           |$quotes
           |I wonder for how much longer
           |$quotes
           |union Kingdom @foo = England | NorthernIreland | Scotland | Wales
          """.stripMargin

      val expected = Schema(
        schemaDefinition = None,
        typeDefinitions = Map(
          Name("Kingdom") -> UnionTypeDefinition(
            Some(Description("I wonder for how much longer")),
            Name("Kingdom"),
            List(Directive(Name("foo"), List.empty)),
            List(
              Name("England"),
              Name("NorthernIreland"),
              Name("Scotland"),
              Name("Wales")
            )
          )
        )
      )

      SchemaParser.parse(input.trim) shouldBe Right(expected)
    }
  }

  "Schema parser: examples" should {

    "Parse the GitHub schema" in {
      SchemaParser.parse(Loader.load("/schemas/github.idl")) should matchPattern { case Right(_) => }
    }

    "Parse the IntelliJ generated Github schema" in {
      val result = SchemaParser.parse(Loader.load("/schemas/github_intellij.idl"))
      result.map(_.schemaDefinition) shouldBe Right(
        Some(
          SchemaDefinition(
            NonEmptyList.of(
              RootOperationTypeDefinition(Query, Name("Query")),
              RootOperationTypeDefinition(Mutation, Name("Mutation")),
            )
          )
        )
      )
    }

    "Parse the Braintree schema" in {
      SchemaParser.parse(Loader.load("/schemas/braintree.idl")) should matchPattern { case Right(_) => }
    }
  }
}
