package recursion

import cats.Functor
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.functor._

object CataAna8 {

  type ListF[E, P] = Option[(E, P)]

  implicit def patternFunctorInstance[E]: Functor[ListF[E, *]] =
    Functor[Option].compose[(E, *)]

  def cata[F[_]: Functor, S, B](algebra: F[B] => B)(project: S => F[S]): S => B =
    new (S => B) {
      kernel =>
      override def apply(list: S): B =
        algebra(project(list).fmap(kernel))
    }

  // TODO repeat for `ana`
  def unfold[E, A](f: A => Option[(E, A)]): A => List[E] =
    new (A => List[E]) {
      kernel =>

      override def apply(a: A): List[E] =
        f(a) match {
          case Some((e, a)) => e :: kernel(a)
          case None => Nil
        }
    }

  val productOp: ListF[Int, Int] => Int =
    _ match {
      case Some((x, y)) => x * y
      case None => 1
    }

  def projectList[E]: List[E] => ListF[E, List[E]] =
    _ match {
      case Nil => Option.empty[(E, List[E])]
      case ::(head, tl) => Option((head, tl))
    }

  val rangeOp: Int => Option[(Int, Int)] =
    x => if (x > 0) Some((x, x - 1)) else None

  def main(args: Array[String]): Unit = {
    val foldF = cata(productOp)(projectList) // (patternFunctorInstance[Int])
    println(foldF(1 :: 10 :: 20 :: Nil))
    println(unfold(rangeOp)(10))
  }
}
