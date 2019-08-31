package io.tvc.graphql.inlining

import io.tvc.graphql.Fix
import io.tvc.graphql.inlining.InputInliner.{InputObject, InputTypeTree, InputValue, RecInputTypeTree}
import io.tvc.graphql.inlining.TypeTree.TypeModifier.NullableType
import io.tvc.graphql.inlining.TypeTree.{Field, FieldName, Metadata}
import io.tvc.graphql.parsing.CommonModel.Type.NamedType
import io.tvc.graphql.parsing.CommonModel.Value.{EnumValue, StringValue}
import io.tvc.graphql.parsing.CommonModel.{Name, Variable}
import io.tvc.graphql.parsing.QueryModel.VariableDefinition
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition.{EnumTypeDefinition, InputObjectTypeDefinition, ScalarTypeDefinition}
import io.tvc.graphql.parsing.SchemaModel.{EnumValueDefinition, InputValueDefinition, Schema}
import org.scalatest.{Matchers, WordSpec}

/**
  * Example based tests like this don't work very well for these giant tree transforms
  * but I can't really assert too many generic properties usefully with such huge structures
  */
class InputInlinerTest extends WordSpec with Matchers {

  "InputInliner" should {

    "Work with no variables" in {
      val expected = TypeTree.Object(Metadata(None, "Inputs"), List.empty)
      InputInliner.run(List.empty, List.empty) shouldBe Right(expected)
    }

    "Recursively inline any references to input objects" in {

      val schema: Schema = List(
        ScalarTypeDefinition(
          description = None,
          name = Name("FooScalar"),
          directives = List.empty
        ),
        EnumTypeDefinition(
          description = None,
          name = Name("Time"),
          directives = List.empty,
          values = List(
            EnumValueDefinition(
              description = None,
              value = EnumValue(Name("Whatever")),
              directives = List.empty
            ),
            EnumValueDefinition(
              description = None,
              value = EnumValue(Name("Whenever")),
              directives = List.empty
            )
          )
        ),
        InputObjectTypeDefinition(
          None,
          Name("Foo"),
          List.empty,
          List(
            InputValueDefinition(
              description = None,
              name = Name("blah"),
              `type` = NamedType(Name("String")),
              defaultValue = Some(StringValue("Blah")),
              directives = List.empty
            ),
            InputValueDefinition(
              description = None,
              name = Name("something"),
              `type` = NamedType(Name("Time")),
              defaultValue = None,
              directives = List.empty
            )
          )
        )
      )

      val arguments: List[VariableDefinition] = List(
        VariableDefinition(Variable("foo"), NamedType(Name("Foo")), default = None),
        VariableDefinition(Variable("bar"), NamedType(Name("FooScalar")), default = Some(StringValue("Blah"))),
      )

      val expected: InputObject[RecInputTypeTree] = TypeTree.Object(
        meta = Metadata(None, "Inputs"),
        fields = List(
          Field(
            name = FieldName(None, "foo"),
            modifiers = List(NullableType),
            `type` = InputValue(
              default = None,
              value = Fix[InputTypeTree](
                TypeTree.Object(
                  meta = Metadata(None, "Foo"),
                  fields = List(
                    Field(
                      name = FieldName(None, "blah"),
                      modifiers = List(NullableType),
                      `type` = InputValue(
                        default = Some("StringValue(Blah)"),
                        value = Fix[InputTypeTree](TypeTree.Scalar(meta = Metadata(None, "String")))
                      )
                    ),
                    Field(
                      name = FieldName(None, "something"),
                      modifiers = List(NullableType),
                      `type` = InputValue(
                        default = None,
                        value = Fix[InputTypeTree](
                          TypeTree.Enum(
                            meta = Metadata(None, "Time"),
                            fields = List("Whatever", "Whenever")
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          Field(
            name = FieldName(None, "bar"),
            modifiers = List(NullableType),
            `type` = InputValue(
              default = Some("StringValue(Blah)"),
              value = Fix[InputTypeTree](
                TypeTree.Scalar(meta = Metadata(None, "FooScalar"))
              )
            )
          )
        )
      )

      InputInliner.run(schema, arguments) shouldBe Right(expected)
    }
  }
}
