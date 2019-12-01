import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.onedot"
ThisBuild / organizationName := "onedot"

lazy val root = (project in file("."))
  .settings(
    name := "csvparser",
    libraryDependencies += scalaTest % Test
  )

