ThisBuild / scalaVersion := "2.13.4"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"
ThisBuild / useCoursier := false

lazy val root = (project in file("."))
  .settings(
    name := "advent",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.1.2" % "test"
    ),
    scalacOptions += "-deprecation"
  )
