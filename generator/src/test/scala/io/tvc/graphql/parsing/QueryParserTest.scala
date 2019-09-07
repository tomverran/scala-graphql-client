package io.tvc.graphql.parsing

import io.tvc.graphql.parsing.CommonModel.OperationType.Query
import io.tvc.graphql.parsing.CommonModel.Type.{ListType, NamedType, NonNullType}
import io.tvc.graphql.parsing.CommonModel.Value.{EnumValue, IntValue, StringValue}
import io.tvc.graphql.parsing.CommonModel.{Argument, Directive, Name, Variable}
import io.tvc.graphql.parsing.QueryModel.Selection.{Field, FragmentSpread, InlineFragment}
import io.tvc.graphql.parsing.QueryModel.{FragmentDefinition, OperationDefinition, SelectionSet, VariableDefinition}
import io.tvc.graphql.parsing.QueryParser.parse
import org.scalatest.{Matchers, WordSpec}

class QueryParserTest extends WordSpec with Matchers {

  "Query parser" should {

    "Parse an empty query" in {
      val expected = OperationDefinition(Query, None, List.empty, SelectionSet(List.empty))
      parse("{}") shouldBe Right(expected)
    }

    "Parse a query with variables" in {

      val variables = List(
        VariableDefinition(Variable("foo"), NonNullType(NamedType(Name("Int"))), default = None),
        VariableDefinition(Variable("baz"), ListType(NonNullType(NamedType(Name("Boolean")))), default = None),
        VariableDefinition(Variable("bar"), NamedType(Name("String")), default = Some(StringValue("def")))
      )

      val qry = """query($foo: Int!, $baz: [Boolean!] $bar: String = "def") {}"""
      val expected = OperationDefinition(Query, None, variables, SelectionSet(List.empty))
      parse(qry) shouldBe Right(expected)
    }

    "Parse a query with a nested selection set" in {

      val qry =
        """
          |{
          |  foo,
          |  bar
          |  animal(name: "bar", age :10 type:CAT) {
          |    vertebrate {
          |      tom:human {
          |        programmer {
          |          name,
          |          address()
          |          phone
          |        }
          |      }
          |    }
          |  }
          |}
        """.stripMargin

      val animalArgs = List(
        Argument(Name("name"), StringValue("bar")),
        Argument(Name("age"), IntValue(10)),
        Argument(Name("type"), EnumValue(Name("CAT"))),
      )

      val expected = OperationDefinition(Query, None, List.empty, SelectionSet(
        List(
          Field(None, Name("foo"), List.empty, None),
          Field(None, Name("bar"), List.empty, None),
          Field(None, Name("animal"), animalArgs, Some(SelectionSet(
            List(
              Field(None, Name("vertebrate"), List.empty, Some(SelectionSet(
                List(
                  Field(Some(Name("tom")), Name("human"), List.empty, Some(SelectionSet(
                    List(
                      Field(None, Name("programmer"), List.empty, Some(SelectionSet(
                        List(
                          Field(None, Name("name"), List.empty, None),
                          Field(None, Name("address"), List.empty, None),
                          Field(None, Name("phone"), List.empty, None),
                        )
                      )))
                    )
                  )))
                )
              )))
            )
          )))
        )
      ))

      parse(qry).map(_.operationDefinition) shouldBe Right(expected)
    }
  }

  "Query parser: fragments" should {

    "Parse a fragment spread" in {
      val query = "query foo { ...fields @foo() }"
      val result = QueryParser.parse(query).map(_.operationDefinition.selectionSet.fields.head)
      result shouldBe Right(FragmentSpread(Name("fields"), List(Directive(Name("foo"), List.empty))))
    }

    "Parse an inline fragment" in {
      val query = "query foo { ... on User { friends { count } } }"
      val result = QueryParser.parse(query).map(_.operationDefinition.selectionSet.fields.head)
      val count = SelectionSet(List(Field(None, Name("count"), List.empty, None)))
      val friends = SelectionSet(List(Field(None, Name("friends"), List.empty, Some(count))))
      result shouldBe Right(InlineFragment(NamedType(Name("User")), List.empty, friends))
    }

    "Parse a fragment definition" in {
      val fragment = "query foo {} fragment bar on Baz { blah }"
      val result = QueryParser.parse(fragment).map(_.fragmentDefinitions)
      val blah = SelectionSet(List(Field(None, Name("blah"), List.empty, None)))
      result shouldBe Right(List(FragmentDefinition(Name("bar"), NamedType(Name("Baz")), List.empty, blah)))
    }
  }

  "Query parser: examples" should {

    "Parse the single repo GitHub query" in {
      QueryParser.parse(Loader.load("/queries/single_repo.graphql")) should matchPattern { case Right(_) => }
    }

    "Parse the all repos GitHub query" in {
      val res = QueryParser.parse(Loader.load("/queries/all_repos.graphql"))
      res should matchPattern { case Right(_) => }
    }
  }
}
