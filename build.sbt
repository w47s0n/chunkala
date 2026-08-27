ThisBuild / organization := "com.w47s0n"
ThisBuild / scalaVersion := "3.9.0"
ThisBuild / versionScheme := Some("early-semver")

ThisBuild / licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/w47s0n/chunkala"))
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/w47s0n/chunkala"), "scm:git:git@github.com:w47s0n/chunkala.git")
)
ThisBuild / developers := List(
  Developer("w47s0n", "Watson Dinh", "ping@w47s0n.com", url("https://github.com/w47s0n"))
)

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-source:3.9",
  "-no-indent",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Werror"
)

lazy val root = project
  .in(file("."))
  .aggregate(core, cli)
  .settings(name := "chunkala-root", publish / skip := true)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "chunkala",
    description := "Syntax-aware chunking of Scala source, for embedding and retrieval.",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "4.17.3",
      "org.scalameta" %% "munit" % "1.3.5" % Test
    )
  )

// Not published: a thin driver for the library, plus the harness hook that diffs
// output against the reference implementation (CocoIndex).
lazy val cli = project
  .in(file("cli"))
  .dependsOn(core)
  .settings(name := "chunkala-cli", publish / skip := true)
