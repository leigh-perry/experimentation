package tech.monoidnthat

import cats.Monoid
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.instances.set._
import cats.instances.tuple._
import cats.syntax.foldable._

object Monoid03Tuple {

  final case class Max(i: Int) extends AnyVal
  implicit val intMaxMonoid =
    new Monoid[Max] {
      override def empty: Max = Max(Int.MinValue)
      override def combine(x: Max, y: Max): Max = if (x.i > y.i) x else y
    }

  final case class Min(i: Int) extends AnyVal
  implicit val intMinMonoid =
    new Monoid[Min] {
      override def empty: Min = Min(Int.MaxValue)
      override def combine(x: Min, y: Min): Min = if (x.i < y.i) x else y
    }

  def main(args: Array[String]): Unit = {

    // (a) products
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

    if (false) {
      val (phraseCount, chars) = phrases.foldMap(s => (1, s.length))
      println(phraseCount)
      println(chars)

      println("===============")
    }

    if (false) {
      val (phraseCount, chars, phraseMap) = phrases.foldMap(s => (1, s.length, Map(s -> 1)))
      println(phraseCount)
      println(chars)
      println(phraseMap)

      println("===============")
    }

    ////

    if (false) {
      val (count, max, min, countMap) =
        phrases.foldMap(
          s => (1, Max(s.length), Min(s.length), Map(s.length -> Set(s)))
        )
      println(count)
      println(max)
      println(min)
      println(countMap)
      println("===============")
    }
  }
}
