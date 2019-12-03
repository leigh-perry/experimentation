package tech.monoidnthat

import cats.instances.int._
import cats.instances.list._
import cats.syntax.foldable._

object Monoid02Demo {

  def main(args: Array[String]): Unit = {

    val phrases =
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

    val chars = phrases.foldMap(_.length)
    println(chars)
  }
}
