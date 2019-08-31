package io.tvc.graphql
import io.tvc.graphql.parsing.{QueryParser, SchemaParser}
import io.tvc.graphql.generation.ScalaCodeGenerator.{Generateable, generate}
import io.tvc.graphql.generation.TypeDeduplicator
import io.tvc.graphql.inlining.{InputInliner, OutputInliner}
import io.tvc.graphql.generation.TypeDeduplicator.deduplicate

object Generator {

  case class GeneratedQueryCode(
    namespace: String,
    name: String,
    code: String
  )

  def generateCode(schema: String, namespace: String, query: String): Either[String, GeneratedQueryCode] =
    for {
      sch <- SchemaParser.parse(schema)
      parsedQuery <- QueryParser.parse(query)
      outputs <- OutputInliner.run(sch, parsedQuery.operation.selectionSet).left.map(te => s"$te")
      inputs <- InputInliner.run(sch, parsedQuery.operation.variableDefinitions).left.map(te => s"$te")
    } yield {
      val queryName = parsedQuery.operation.name.fold("AnonymousQuery")(_.value.capitalize)
      val TypeDeduplicator.Output(types, vars) = deduplicate(inputs, outputs)
      val generateable = Generateable(parsedQuery.operation.toString, queryName, namespace, parsedQuery.stringValue, types, vars)
      GeneratedQueryCode(namespace, queryName, generate(generateable))
    }
}
