package io.tvc.graphql.parsing

import io.tvc.graphql.parsing.CommonModel._

object QueryModel {

  /**
   *  https://graphql.github.io/graphql-spec/June2018/#sec-Language.Operations
   */
  sealed trait OperationType

  object OperationType {
    case object Query extends OperationType
    case object Mutation extends OperationType
    case object Subscription extends OperationType
  }

  /**
   * https://graphql.github.io/graphql-spec/June2018/#VariableDefinition
   */
  case class VariableDefinition(variable: Variable, `type`: Type, default: Option[Value])

  /**
   * https://graphql.github.io/graphql-spec/June2018/#VariableDefinitions
   */
  case class VariableDefinitions(value: List[VariableDefinition])

  /**
   * https://graphql.github.io/graphql-spec/June2018/#OperationDefinition
   */
  case class OperationDefinition(
    operationType: OperationType,
    name: Option[Name],
    variableDefinitions: List[VariableDefinition],
    selectionSet: SelectionSet
  )

  /**
   * https://graphql.github.io/graphql-spec/June2018/#SelectionSet
   */
  case class SelectionSet(fields: List[Field])

  /**
   * https://graphql.github.io/graphql-spec/June2018/#Field
   */
  case class Field(
    alias: Option[Name],
    name: Name,
    arguments: List[Argument],
    selectionSet: Option[SelectionSet]
  )
}
