package io.tvc.graphql

import io.tvc.graphql.CommonModel._
import io.tvc.graphql.CommonModel.Value._

object SchemaModel {

  /**
   * https://graphql.github.io/graphql-spec/June2018/#Description
   */
  case class Description(value: String) extends AnyVal {
    override def toString: String = value.replace('\n', ' ').take(10) + "..."
  }

  type Desc = Option[Description]
  type Directives = List[Directive]

  /**
   * https://graphql.github.io/graphql-spec/June2018/#EnumValuesDefinition
   */
  case class EnumValueDefinition(description: Desc, value: EnumValue, directives: List[Directive])

  /**
   * https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
   */
  case class InputValueDefinition(
    description: Desc,
    name: Name,
    `type`: Type,
    defaultValue: Option[Value],
    directives: Directives
  )

  /**
   * https://graphql.github.io/graphql-spec/June2018/#FieldDefinition
   */
  case class FieldDefinition(
    description: Desc,
    name: Name,
    arguments: List[InputValueDefinition],
    `type`: Type,
    directives: Directives
  )

  /**
   * https://graphql.github.io/graphql-spec/June2018/#TypeDefinition
   * I think it might be nice to diverge from the schema to make these less huge
   */
  sealed trait TypeDefinition

  object TypeDefinition {

    case class ScalarTypeDefinition(
      description: Desc,
      name: Name,
      directives: Directives
    ) extends TypeDefinition

    case class ObjectTypeDefinition(
      description: Option[Description],
      name: Name,
      implements: List[Name],
      directives: List[Directive],
      values: List[FieldDefinition]
    ) extends TypeDefinition

    case class InterfaceTypeDefinition(
      description: Desc,
      name: Name,
      directives: List[Directive],
      values: List[FieldDefinition]
    )

    case class UnionTypeDefinition(
      description: Desc,
      name: Name,
      directives: Directives,
      values: List[Name]
    )

    case class EnumTypeDefinition(
      description: Desc,
      name: Name,
      directives: Directives,
      values: List[EnumValueDefinition]
    )

    case class InputObjectTypeDefinition(
      description: Desc,
      name: Name,
      directive: Directives,
      values: List[InputValueDefinition]
    )
  }
}
