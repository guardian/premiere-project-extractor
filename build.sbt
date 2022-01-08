name := "prem test"

version := "0.1"

scalaVersion := "2.13.6"

val AkkaVersion = "2.6.14"
libraryDependencies ++= Seq(
  "com.lightbend.akka" %% "akka-stream-alpakka-xml" % "3.0.1",
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion
)

val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)