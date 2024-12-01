import Dependencies._

ThisBuild / scalaVersion := "3.5.2"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "net.dodropin"
ThisBuild / organizationName := "dodropin"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case _                             => MergeStrategy.first
}

lazy val root = (project in file("."))
  .settings(
    name := "calc",
    libraryDependencies += munit % Test,
    mainClass := Some("dodropin.Calc"),
    scalacOptions ++= Seq( // use ++= to add to existing options
      "-encoding",
      "utf8", // if an option takes an arg, supply it on the same line
      "-feature", // then put the next option on a new line for easy editing
      "-unchecked",
      "-Wunused:imports,privates,locals,implicits",
      "-Werror"
    )
  )
