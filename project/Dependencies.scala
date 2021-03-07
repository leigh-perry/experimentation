import sbt._
object Dependencies {
  object Version {
    val kindProjector = "0.11.3"

    val slf4j = "1.7.30"
    val pprint = "0.6.1"
    val scalazCore = "7.3.3"

    val catsCore = "2.4.2"
    val catsFree = catsCore
    val catsMtlCore = "0.7.1"
    val catsEffect = "2.3.3"

    //val iota = "0.3.10"
    val scalacheck = "1.15.3"
    val minitest = "2.9.3"
    val minitestLaws = minitest

    val fs2Core = "2.5.3"
    val fs2IO = fs2Core

    //val reftree = "1.4.0"
    val monocle = "2.1.0"
  }

  val slf4j = "org.slf4j" % "slf4j-api" % Version.slf4j
  val pprint = "com.lihaoyi" %% "pprint" % Version.pprint
  val scalazCore = "org.scalaz" %% "scalaz-core" % Version.scalazCore
  val catsCore = "org.typelevel" %% "cats-core" % Version.catsCore
  val catsFree = "org.typelevel" %% "cats-free" % Version.catsFree
  val catsMtlCore = "org.typelevel" %% "cats-mtl-core" % Version.catsMtlCore
  val catsEffect = "org.typelevel" %% "cats-effect" % Version.catsEffect
  //val iota = "io.frees" %% "iota-core" % Version.iota
  val scalacheck = "org.scalacheck" %% "scalacheck" % Version.scalacheck
  val minitest = "io.monix" %% "minitest" % Version.minitest
  val minitestLaws = "io.monix" %% "minitest-laws" % Version.minitestLaws
  val fs2Core = "co.fs2" %% "fs2-core" % Version.fs2Core
  val fs2IO = "co.fs2" %% "fs2-io" % Version.fs2IO
  //val reftree = "io.github.stanch" %% "reftree" % Version.reftree
  val monocle = "com.github.julien-truffaut" %% "monocle-core" % Version.monocle
}
