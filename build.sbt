name := "scala-graphql-client"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions ++= Seq(
  "-unchecked", "-deprecation", "-encoding", "utf8", "-feature", "-Xfatal-warnings", "-Ypartial-unification"
)

libraryDependencies ++= List(
  "org.tpolecat" %% "atto-core" % "0.6.5"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
