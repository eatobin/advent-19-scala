ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.eatobin"
ThisBuild / organizationName := "eatobin"
ThisBuild / useCoursier := true

lazy val root = (project in file("."))
  .settings(
    name := "advent-19-scala",
    libraryDependencies ++= Seq(
      "com.lihaoyi" % "ammonite" % "2.5.5" % "test" cross CrossVersion.full
    ),
    scalacOptions += "-deprecation"
  )

// sourceGenerators in Test += Def.task {
Test / sourceGenerators += Def.task {
  val file = (Test / sourceManaged).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.AmmoniteMain.main(args) }""")
  Seq(file)
}.taskValue
