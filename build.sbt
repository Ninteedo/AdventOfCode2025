ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2025"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
libraryDependencies += "com.lihaoyi" %% "upickle" % "4.4.1"
libraryDependencies += "com.google.ortools" % "ortools-java" % "9.14.6206"
