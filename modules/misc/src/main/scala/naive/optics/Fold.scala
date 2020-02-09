package naive.optics

import cats.Monoid
import cats.instances.string._
import cats.instances.option._
import cats.instances.list._
import cats.syntax.foldable._

abstract class Fold[S, A] {
  self =>

  // underlying representation of [[Fold]], all [[Fold]] methods are defined in terms of foldMap
  def foldMap[M: Monoid](f: A => M)(s: S): M

  def composeFold[B](other: Fold[A, B]): Fold[S, B] =
    ???
}

object Fold {}

////

object FoldTest {
  def main(args: Array[String]): Unit = {
    val fo: Fold[Option[Int], Int] =
      new Fold[Option[Int], Int] {
        override def foldMap[M: Monoid](f: Int => M)(s: Option[Int]): M =
          s.foldMap(f)
      }

    println(fo.foldMap[String](n => n.toString * n)(Option(3)))
    println(fo.foldMap[String](n => n.toString * n)(None))

    val fl: Fold[List[Int], Int] =
      new Fold[List[Int], Int] {
        override def foldMap[M: Monoid](f: Int => M)(s: List[Int]): M =
          s.foldMap(f)
      }

    println(fl.foldMap[String](n => n.toString * n)(List(1, 2, 3, 4, 5, 6, 7)))
  }
}
