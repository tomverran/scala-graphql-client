package io.tvc.graphql
import java.nio.file.{Files, Paths}

import atto.syntax.parser._
import io.tvc.graphql.parsing.QueryParser.operationDefinition
import io.tvc.graphql.parsing.SchemaParser.schema
import io.tvc.graphql.transform.{ScalaCodeGen, TypeChecker, TypeDeduplicator}

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
    query <- operationDefinition.parseOnly(load("/queries/query.graphql")).either
    tree <- TypeChecker.run(sch, query).left.map(te => s"$te")
  } yield Files.write(Paths.get("output.scala"), ScalaCodeGen.generate(TypeDeduplicator.run(tree)).getBytes)

}
