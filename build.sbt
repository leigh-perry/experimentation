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

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.10" cross CrossVersion.binary)
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.26"

//libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.0"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.4"
//libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.0-M4-pre-20d3c21"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M30"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.6.1"
libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.5.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.1"

libraryDependencies += "io.frees" %% "iota-core" % "0.3.10"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" /*% "test"*/
libraryDependencies += "io.monix" %% "minitest" % "2.4.0" /*% "test"*/
libraryDependencies += "io.monix" %% "minitest-laws" % "2.4.0" /*% "test"*/

libraryDependencies += "io.github.stanch" %% "reftree" % "1.4.0"
libraryDependencies += "co.fs2" %% "fs2-core" % "1.0.3"
libraryDependencies += "co.fs2" %% "fs2-io" % "1.0.3"
