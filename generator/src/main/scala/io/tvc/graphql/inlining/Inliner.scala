package io.tvc.graphql.inlining

import io.tvc.graphql.parsing.QueryModel.OperationDefinition
import io.tvc.graphql.parsing.SchemaModel.Schema
import io.tvc.graphql.inlining.Utilities.TypeError.OrMissing
import io.tvc.graphql.inlining.InputInliner.RecInputTypeTree
import io.tvc.graphql.inlining.TypeTree.RecTypeTree
import cats.instances.either._
import cats.syntax.apply._

/**
  * Given a query and schema def, zip the two together and inline all the types
  * to produce a series of recursive ASTs for both the query inputs and the outputs
  * This is so later steps can deal with duplicate names etc later
  */
object Inliner {

  case class InlinedQuery(
    inputs: List[RecInputTypeTree],
    output: RecTypeTree
  )

  def apply(schema: Schema, operationDefinition: OperationDefinition): OrMissing[InlinedQuery] =
    (
      InputInliner.run(schema, operationDefinition),
      OutputInliner.run(schema, operationDefinition)
    ).mapN(InlinedQuery.apply)

}
