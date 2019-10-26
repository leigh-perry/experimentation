name := "experimentation"
version := "1.0.1-SNAPSHOT"

scalaVersion := "2.12.8"
//scalacOptions := Seq("-unchecked", "-deprecation", "-Xexperimental", "-feature")
scalacOptions ++=
  Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    //"-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture",
    "-Yrangepos",
    "-target:jvm-1.8",
    "-Xexperimental"
  )

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3" /* cross CrossVersion.binary*/)
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.28"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.5"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M31"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.0.0"
libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.7.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"

libraryDependencies += "io.frees" %% "iota-core" % "0.3.10"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.2" /*% "test"*/
libraryDependencies += "io.monix" %% "minitest" % "2.7.0" /*% "test"*/
libraryDependencies += "io.monix" %% "minitest-laws" % "2.7.0" /*% "test"*/

libraryDependencies += "io.github.stanch" % "reftree_2.12" % "1.4.0"
libraryDependencies += "co.fs2" %% "fs2-core" % "2.0.1"
libraryDependencies += "co.fs2" %% "fs2-io" % "2.0.1"
