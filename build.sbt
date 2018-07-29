name := "experimentation"
version := "1.0.1-SNAPSHOT"

scalaVersion := "2.12.6"
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

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.7" cross CrossVersion.binary)

libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3"
//libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.0-M4-pre-20d3c21"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M24"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.2.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0-RC2-d7181dc"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" /*% "test"*/

