package exp

import cats.Monoid
import cats.implicits._

object Monoids {
  // List[A]
  // A => B     if B is monoid (mempty is id, mappend = .)
  // (A, B)     if A and B are monoids
  // Future[A]  if A is monoid
  // Map[A, B]

  def main(args: Array[String]): Unit = {

    // Config => Future[Map[String, (Set[A], Option[B])]]
    val list =
      List("Hi my name is Ingrid what is your name")
        .flatMap(_.split("""\W+"""))

    if (true) {
      val (words, chars, wordMap) = list.foldMap(word => (1, word.length, Map(word -> 1)))
      println(words)
      println(chars)
      println(wordMap)
    }

    ////

    final case class Max(i: Int)
    implicit val intMaxMonoid =
      new Monoid[Max] {
        override def empty: Max = Max(Int.MinValue)
        override def combine(x: Max, y: Max): Max = if (x.i > y.i) x else y
      }

    final case class Min(i: Int)
    implicit val intMinMonoid =
      new Monoid[Min] {
        override def empty: Min = Min(Int.MaxValue)
        override def combine(x: Min, y: Min): Min = if (x.i < y.i) x else y
      }

    if (true) {
      val (count, max, min, countMap) =
        list.foldMap(word => (1, Max(word.length), Min(word.length), Map(word.length -> Set(word))))
      println(count)
      println(max)
      println(min)
      println(countMap)
    }
  }
}
