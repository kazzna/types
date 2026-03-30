ThisBuild / organization := "jp.kazzna"
ThisBuild / scalaVersion := "3.8.3"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/kazzna/types")
ThisBuild / credentials ++= (sys.env.get("GITHUB_TOKEN") match {
  case Some(token) => Seq(Credentials("GitHub Package Registry", "maven.pkg.github.com", "kazzna", token))
  case None => Seq()
})

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
      "org.scalatest" %% "scalatest" % "3.2.20" % "test"
    )
  )
