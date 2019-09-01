package io.tvc.graphql
import io.tvc.graphql.parsing.{QueryParser, SchemaParser}
import io.tvc.graphql.generation.ScalaCodeGenerator.{GenerationInput, Query, generate}
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
      outputs <- OutputInliner.run(sch, parsedQuery.selectionSet).left.map(te => s"$te")
      inputs <- InputInliner.run(sch, parsedQuery.variableDefinitions).left.map(te => s"$te")
    } yield {
      val deduped: TypeDeduplicator.Output = deduplicate(inputs, outputs)
      val queryName = parsedQuery.name.fold("AnonymousQuery")(_.value.capitalize)
      GeneratedQueryCode(
        namespace = namespace,
        name = queryName,
        code = generate(
          GenerationInput(
            namespace,
            deduped.root,
            deduped.types,
            query = Query(
              name = queryName,
              stringValue = query,
              inputs = deduped.variables
            )
          )
        )
      )
    }
}
