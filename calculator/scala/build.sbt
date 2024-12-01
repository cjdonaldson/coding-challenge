import Dependencies._

ThisBuild / scalaVersion := "3.5.2"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "net.dodropin"
ThisBuild / organizationName := "dodropin"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x                             => MergeStrategy.first
}

lazy val root = (project in file("."))
  .settings(
    name := "calc",
    libraryDependencies += munit % Test,
    // scalacOptions += "-explain"
    scalacOptions += "-Yrangepos",
    mainClass := Some("dodropin.Calc")
  )
