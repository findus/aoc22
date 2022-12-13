ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "aoc22"
  )

libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1") // for scalameter 2.13
