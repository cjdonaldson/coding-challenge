import Dependencies._

ThisBuild / scalaVersion := "3.5.2"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "net.dodropin"
ThisBuild / organizationName := "dodropin"

lazy val root = (project in file("."))
  .settings(
    name := "my-calc-cli",
    libraryDependencies += munit % Test,
    scalacOptions += "-explain"
  )
