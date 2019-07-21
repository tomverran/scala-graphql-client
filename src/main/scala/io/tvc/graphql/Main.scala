package io.tvc.graphql
import atto.syntax.parser._
import io.tvc.graphql.parsing.QueryParser.operationDefinition
import io.tvc.graphql.parsing.SchemaParser.schema
import io.tvc.graphql.transform.TypeChecker

import scala.io.Source

object Main extends App {

  def load(file: String): String = {
    val source = Source.fromURL(getClass.getResource(file))
    val lines = source.getLines.mkString("\n")
    source.close
    lines
  }

  for {
    sch <- schema.parseOnly(load("/schemas/schema.idl")).option
    query <- operationDefinition.parseOnly(load("/queries/query.graphql")).option
  } yield TypeChecker.run(sch, query).fold(println(_), println(_))

}
