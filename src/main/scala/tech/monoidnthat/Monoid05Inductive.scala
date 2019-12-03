package tech.monoidnthat

import cats.Semigroup
import cats.instances.list._
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.foldable._

object Monoid05Inductive {

  final case class SgMax(i: Int) extends AnyVal
  implicit val intSgMaxSemigroup =
    new Semigroup[SgMax] {
      override def combine(x: SgMax, y: SgMax): SgMax = if (x.i > y.i) x else y
    }

  final case class SgMin(i: Int) extends AnyVal
  implicit val intSgMinSemigroup =
    new Semigroup[SgMin] {
      override def combine(x: SgMin, y: SgMin): SgMin = if (x.i < y.i) x else y
    }

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

    // (b) inductively

    // eg Option[A] is monoid if A is a semigroup
    //val nelMonoid1 = Monoid[Option[NonEmptyList[String]]]

    // calculate min and max without reserving in-band values Int.MaxValue or Int.MinValue
    if (true) {
      val (max, min) =
        phrases.foldMap(
          s => Option(SgMax(s.length)) -> Option(SgMin(s.length))
        )
      println(max)
      println(min)
    }
  }
}
