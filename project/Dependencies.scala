import sbt._

object Dependencies {

  val http4sVersion = "0.18.21"
  val monocleVersion = "1.4.0"
  val circeVersion = "0.9.3"
  //test libs
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.1"
  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.5"

  // scala prod libs
  val http4sLib = "org.http4s" %% "http4s-core" % http4sVersion
  val http4sDsl = "org.http4s" %% "http4s-dsl" % http4sVersion
  val http4sServer = "org.http4s" %% "http4s-blaze-server" % http4sVersion
  //val http4sArgonaut = "org.http4s" %% "http4s-argonaut" % http4sVersion
  val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
//  val argonaut = "io.argonaut" %% "argonaut" % "6.2"
  val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13"
  val cats = "org.typelevel" %% "cats-core" % "1.1.0"
  val refined = "eu.timepit" %% "refined" % "0.9.4"

  // Java libs
  val postgres = "org.postgresql" % "postgresql" % "42.1.4"
  val hikari = "com.zaxxer" % "HikariCP" % "2.7.4"
  val logback = "ch.qos.logback" % "logback-core" % "1.2.3"
  val slf4j = "org.slf4j" % "slf4j-api" % "1.7.25"
  val slf4jSimple = "org.slf4j" % "slf4j-simple" % "1.7.25"
  val flyway = "org.flywaydb" % "flyway-core" % "5.0.0"
  val solrj = "org.apache.solr" % "solr-solrj" % "6.6.1"
  val scrypt = "com.lambdaworks" % "scrypt" % "1.4.0"
//  val itext = "com.itextpdf" % "itextpdf" % "5.5.12"


  val circeCore = "io.circe" %% "circe-core" % circeVersion
  val circeParser = "io.circe" %% "circe-parser" % circeVersion

  val monocle = "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion
  val monocleMacro = "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion


}
