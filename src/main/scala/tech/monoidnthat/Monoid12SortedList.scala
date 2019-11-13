package tech.monoidnthat

import cats.effect.{IO, Timer}
import cats.{Monoid, Show}

object Monoid12SortedList {

  final case class SortedList[A: Ordering](list: List[A])

  object SortedList {
    implicit def showInstance[A: Ordering]: Show[SortedList[A]] =
      new Show[SortedList[A]] {
        override def show(t: SortedList[A]): String =
          t.toString
      }
  }

  implicit def sortedListMonoid[A: Ordering]: Monoid[SortedList[A]] =
    new Monoid[SortedList[A]] {
      override def empty: SortedList[A] =
        SortedList(List.empty[A])

      override def combine(x: SortedList[A], y: SortedList[A]): SortedList[A] =
        SortedList(mergeSort(x.list, y.list))
    }

  def mergeSort[A: Ordering](x: List[A], y: List[A]): List[A] =
    (x, y) match {
      case (Nil, y) => y
      case (x, Nil) => x
      case (xh :: xt, yh :: yt) if Ordering[A].compare(xh, yh) < 1 =>
        xh :: mergeSort(xt, y)
      case (xh :: xt, yh :: yt) =>
        yh :: mergeSort(x, yt)
    }

  def main(args: Array[String]): Unit = {
    import fs2.Stream

    import scala.concurrent.ExecutionContext
    import scala.concurrent.duration.DurationInt
    implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

    val count = 10
    Stream
      .fixedRate[IO](500.milliseconds)
      .zip(Stream.range[IO](1, count))
      .map(count - _._2)
      .map(i => SortedList(List(i)))
      .scanMonoid
      .showLinesStdOut
      .compile
      .drain
      .unsafeRunSync()
  }
}
