package tech.monoidnthat

import cats.instances.int._
import cats.instances.list._
import cats.syntax.foldable._

object Monoid3Demo {

  def main(args: Array[String]): Unit = {

    val strings =
      List(
        "Some",
        "Phrases",
        "My hovercraft is full of eels",
        "I am no longer infected",
        "A",
        "I",
        "A",
        "I"
      )

    val chars = strings.foldMap(_.length)
    println(chars)
  }
}
