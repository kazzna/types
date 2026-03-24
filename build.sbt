ThisBuild / organization := "jp.kazzna"
ThisBuild / scalaVersion := "3.8.2"

lazy val root = (project in file("."))
  .settings(
    name := "types",
    version := "0.1.0-SNAPSHOT",
    scalacOptions ++= Seq(
      "--deprecation",
      "--feature",
      "--unchecked"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % "test"
    )
  )
