package io.tvc.graphql.parsing

import io.tvc.graphql.parsing.CommonModel.Type.NamedType
import io.tvc.graphql.parsing.CommonModel._

object QueryModel {

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
  case class SelectionSet(fields: List[Selection])

  sealed trait Selection

  object Selection {

    /**
      * https://graphql.github.io/graphql-spec/June2018/#Field
      */
    case class Field(
      alias: Option[Name],
      name: Name,
      arguments: List[Argument],
      selectionSet: Option[SelectionSet]
    ) extends Selection

    /**
      * https://graphql.github.io/graphql-spec/June2018/#FragmentSpread
      */
    case class FragmentSpread(
      name: Name,
      directives: List[Directive]
    ) extends Selection

    /**
      * https://graphql.github.io/graphql-spec/June2018/#InlineFragment
      */
    case class InlineFragment(
      typeCondition: NamedType,
      directives: List[Directive],
      selectionSet: SelectionSet
    ) extends Selection
  }

  /**
    * https://graphql.github.io/graphql-spec/June2018/#FragmentDefinition
    */
  case class FragmentDefinition(
    fragmentName: Name,
    typeCondition: NamedType,
    directives: List[Directive],
    selectionSet: SelectionSet
  )

  /**
    * https://graphql.github.io/graphql-spec/June2018/#ExecutableDefinition
    * We modify this a bit to ensure we have exactly one operation to run
    */
  case class ExecutableDefinition(
    operationDefinition: OperationDefinition,
    fragmentDefinitions: List[FragmentDefinition]
  )
}
