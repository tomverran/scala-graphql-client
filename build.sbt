ThisBuild / scalaVersion := "2.12.9"
ThisBuild / organization := "io.tvc"

/**
  * Libraries and types that the generated code needs to work,
  * i.e. cats and circe as we need to specify how to decode results from JSON
  */
val circeVersion = "0.11.1"
val enumeratumVersion = "1.5.13"
val enumeratumCirceVersion = "1.5.21"
val runtime = (project in file("runtime"))
  .settings(
    name := "runtime",
    libraryDependencies ++= List(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "com.beachape" %% "enumeratum" % enumeratumVersion,
      "com.beachape" %% "enumeratum-circe" % enumeratumCirceVersion,
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
    )
  )

/**
  * The part of the project that parses schemas + queries
  * and outputs scala code that depends on the runtime
  */
val generator = (project in file("generator"))
  .settings(
    name := "generator",
    libraryDependencies ++= List(
      "org.typelevel" %% "kittens" % "1.2.1",
      "org.tpolecat" %% "atto-core" % "0.6.5",
      "io.higherkindness" %% "droste-core" % "0.7.0",
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
    )
  )

/**
  * SBT plugin that lets you generate code at compile time
  * from GraphQL queries and schemas included in your project
  */
val plugin = (project in file("plugin"))
  .enablePlugins(SbtPlugin, BuildInfoPlugin)
  .dependsOn(generator)
  .settings(
    name := "plugin",
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "buildinfo"
  )

val root = (project in file("."))
  .aggregate(plugin, generator, runtime)
  .settings(name := "graphql-client")
  .dependsOn(plugin)

