ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "io.tvc"

ThisBuild / scalacOptions ++= Seq(
  "-unchecked", "-deprecation", "-encoding", "utf8", "-feature", "-Xfatal-warnings", "-Ypartial-unification"
)

val generator = (project in file("generator"))
  .settings(
    name := "generator",
    libraryDependencies ++= List(
      "org.tpolecat" %% "atto-core" % "0.6.5",
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
    )
  )

val plugin = (project in file("plugin"))
  .enablePlugins(SbtPlugin)
  .dependsOn(generator)
  .settings(name := "plugin")

val root = (project in file("."))
  .aggregate(plugin, generator)
  .settings(name := "graphql-client")
  .dependsOn(plugin)

