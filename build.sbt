ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(name := "methodOverrider")

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.10"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test