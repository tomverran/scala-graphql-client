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
      executable <- QueryParser.parse(query)
      parsed = executable.operationDefinition
      outputs <- OutputInliner.run(sch, parsed.operationType, parsed.selectionSet).left.map(te => s"$te")
      inputs <- InputInliner.run(sch, parsed.variableDefinitions).left.map(te => s"$te")
    } yield {
      val deduped: TypeDeduplicator.Output = deduplicate(inputs, outputs)
      val queryName = parsed.name.fold("AnonymousQuery")(_.value.capitalize)
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
