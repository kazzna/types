ThisBuild / organization := "jp.kazzna"
ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "types",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "--deprecation",
      "--feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.15" % "test"
    ),
    Test / testOptions += Tests.Argument("-l", "org.scalatest.tags.Slow")
  )

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
