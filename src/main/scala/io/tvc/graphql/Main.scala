package io.tvc.graphql
import java.nio.file.{Files, Paths}

import atto.syntax.parser._
import io.tvc.graphql.parsing.QueryParser.operationDefinition
import io.tvc.graphql.parsing.SchemaParser.schema
import io.tvc.graphql.transform.ScalaCodeGen.generate
import io.tvc.graphql.transform.TypeChecker
import io.tvc.graphql.transform.TypeDeduplicator.deduplicate

import scala.io.Source

object Main extends App {

  def load(file: String): String = {
    val source = Source.fromURL(getClass.getResource(file))
    val lines = source.getLines.mkString("\n")
    source.close
    lines
  }

  for {
    sch <- schema.parseOnly(load("/schemas/schema.idl")).either
    queryStr = load("/queries/query.graphql")
    query <- operationDefinition.parseOnly(queryStr).either
    name = query.name.fold("AnonymousQuery")(_.value.capitalize)
    tree <- TypeChecker.run(sch, query).left.map(te => s"$te")
  } yield Files.write(Paths.get(s"$name.scala"), generate( name, queryStr, deduplicate(tree) ).getBytes)

}
