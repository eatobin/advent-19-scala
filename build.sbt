ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"
ThisBuild / useCoursier := true

lazy val root = (project in file("."))
  .settings(
    name := "advent-19-scala",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.16.0" % Test
    ),
    scalacOptions += "-deprecation"
  )
