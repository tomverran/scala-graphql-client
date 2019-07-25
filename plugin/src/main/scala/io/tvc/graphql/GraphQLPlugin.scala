package io.tvc.graphql
import java.nio.file.Files

import buildinfo.BuildInfo
import sbt.Keys._
import sbt._

import scala.collection.JavaConverters._
import scala.util.Try

object GraphQLPlugin extends AutoPlugin {

  case class Queries(
    namespace: String,
    schema: File,
    query: File,
  )

  object autoImport {
    val graphQLQueries = settingKey[List[Queries]]("Locations of schemas + queries")
  }

  import autoImport._
  val runGeneration = Def.task {

    def fetchSchema(q: Queries): Either[String, String] =
      Try(Files.readAllLines(q.schema.toPath).asScala.mkString("\n")).toEither.left.map(_.getMessage)

    def fetchQuery(q: Queries): Either[String, String] =
      Try(Files.readAllLines(q.query.toPath).asScala.mkString("\n")).toEither.left.map(_.getMessage)

    graphQLQueries.value.map { q =>
      val srcManaged = (sourceManaged in Compile).value
      val output = srcManaged / q.namespace.replace('.', '/')
      Files.createDirectories(output.toPath)
      (
        for {
          query <- fetchQuery(q)
          schema <- fetchSchema(q)
          code <- Generator.generateCode(schema, q.namespace, query)
          outFile = output / s"${code.name}.scala"
          _ <- Try(Files.write(outFile.toPath, code.code.getBytes)).toEither.left.map(_.getMessage)
        } yield outFile
      ).fold(s => throw new Exception(s), identity)
    }
  }

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    libraryDependencies += "io.tvc" %% "runtime" % BuildInfo.version,
    ThisBuild / graphQLQueries := List.empty[Queries],
    Compile   / sourceGenerators += runGeneration,
  )
}
