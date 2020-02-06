import Dependencies._

val Scala_213 = "2.13.1"
val Scala_212 = "2.12.10"
//val Scala_211 = "2.11.12"

////

val projectName = "fp-experimentation"
val organisationName = "leighperry"

lazy val compilerPlugins =
  List(
    compilerPlugin("org.typelevel" %% "kind-projector" % Version.kindProjectorVersion)
  )

lazy val commonSettings =
  Seq(
    scalaVersion := Scala_212,
    scalacOptions ++= commonScalacOptions(scalaVersion.value),
    fork in Test := true,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    name := projectName,
    organization := organisationName,
    updateOptions := updateOptions.value.withGigahorse(false),
    libraryDependencies ++=
      Seq(
        //zioTest % Test,
        //zioTestSbt % Test
      ) ++ compilerPlugins
  )

lazy val crossBuiltCommonSettings = commonSettings ++ Seq(
  crossScalaVersions := Seq(Scala_212, Scala_213)
)

lazy val misc =
  module("misc")
    .settings(
      libraryDependencies ++=
        Seq(
          slf4j,
          pprint,
          scalazCore,
          catsCore,
          catsFree,
          catsMtlCore,
          catsEffect,
          iota,
          scalacheck,
          minitest,
          minitestLaws,
          fs2Core,
          fs2IO,
          reftree,
          monocle
        )
    )

lazy val `fp-course` =
  module("fp-course")
    .settings(
      libraryDependencies ++=
        Seq(
        )
    )

lazy val allModules = List(misc, `fp-course`)

lazy val root =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(skip in publish := true, crossScalaVersions := List())
    .aggregate(allModules.map(x => x: ProjectReference): _*)
    .dependsOn(allModules.map(x => x: ClasspathDep[ProjectReference]): _*)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fmtcheck", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

////

def module(moduleName: String): Project =
  Project(moduleName, file("modules/" + moduleName))
    .settings(crossBuiltCommonSettings)
    .settings(name += s"-$moduleName")

def versionDependentExtraScalacOptions(scalaVersion: String) =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 =>
      Seq("-Yno-adapted-args", "-Xfuture", "-Ypartial-unification")
    case _ => Nil
  }

def commonScalacOptions(scalaVersion: String) =
  Seq(
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    //"-Xfatal-warnings",
    "-deprecation",
    "-Xlint:-unused,_"
  ) ++
    versionDependentExtraScalacOptions(scalaVersion)

val testDependencies = "compile->compile;test->test"
