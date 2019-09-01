package io.tvc.graphql.inlining

import cats.instances.either._
import cats.syntax.apply._
import io.tvc.graphql.inlining.InputInliner.{InputObject, RecInputTypeTree}
import io.tvc.graphql.inlining.TypeTree.RecTypeTree
import io.tvc.graphql.inlining.Utilities.TypeError.OrMissing
import io.tvc.graphql.parsing.QueryModel.OperationDefinition
import io.tvc.graphql.parsing.SchemaModel.Schema

/**
  * Given a query and schema def, zip the two together and inline all the types
  * to produce a series of recursive ASTs for both the query inputs and the outputs
  * This is so later steps can deal with duplicate names etc later
  */
object Inliner {

  case class InlinedQuery(
    inputs: Option[InputObject[RecInputTypeTree]],
    output: TypeTree.Object[RecTypeTree]
  )

  def apply(schema: Schema, operationDefinition: OperationDefinition): OrMissing[InlinedQuery] =
    (
      InputInliner.run(schema, operationDefinition.variableDefinitions),
      OutputInliner.run(schema, operationDefinition.selectionSet)
    ).mapN(InlinedQuery.apply)

}
