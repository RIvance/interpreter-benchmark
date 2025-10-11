ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "stlc-interpreter"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
