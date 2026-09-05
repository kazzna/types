ThisBuild / organization := "jp.kazzna"
ThisBuild / scalaVersion := "3.9.0"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / publishTo := sys.env.get("GITHUB_PACKAGES_MAVEN_URL").map { url =>
  "GitHub Package Registry" at url
}
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "1.0" / "ghpackages.credentials")

lazy val root = (project in file("."))
  .settings(
    name := "types",
    version := "0.1.1-SNAPSHOT",
    scalacOptions ++= Seq(
      "--deprecation",
      "--feature",
      "--unchecked"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.20" % "test"
    )
  )
