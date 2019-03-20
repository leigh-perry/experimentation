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

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.9" cross CrossVersion.binary)
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.0"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3"
//libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.0-M4-pre-20d3c21"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M27"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.4.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"

libraryDependencies += "io.frees" %% "iota-core" % "0.3.10"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" /*% "test"*/
