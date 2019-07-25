package io.tvc.graphql
import io.tvc.graphql.parsing.{QueryParser, SchemaParser}
import io.tvc.graphql.transform.ScalaCodeGen.generate
import io.tvc.graphql.transform.TypeChecker
import io.tvc.graphql.transform.TypeDeduplicator.deduplicate

object Generator {

  case class GeneratedQueryCode(
    namespace: String,
    name: String,
    code: String
  )

  def generateCode(schema: String, namespace: String, query: String): Either[String, GeneratedQueryCode] =
    for {
      sch <- SchemaParser.parse(schema)
      opDefinition <- QueryParser.parse(query)
      name = opDefinition.name.fold("AnonymousQuery")(_.value.capitalize)
      tree <- TypeChecker.run(sch, opDefinition).left.map(te => s"$te")
    } yield GeneratedQueryCode(namespace, name, generate(name, namespace, query, deduplicate(tree)))
}
