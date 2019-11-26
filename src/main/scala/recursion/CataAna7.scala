package recursion

import cats.Functor
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.functor._

object CataAna7 {

  type ListF[E, P] = Option[(E, P)]

  implicit def patternFunctorInstance[E]: Functor[ListF[E, *]] =
    Functor[Option].compose[(E, *)]

  def foldRight[F[_]: Functor, S, B](f: F[B] => B)(project: S => F[S]): S => B =
    new (S => B) {
      kernel =>

      def step2: F[S] => F[B] =
        _.fmap(kernel)

      def step3: F[B] => B =
        f

      override def apply(list: S): B =
        step3(step2(project(list)))
    }

  def unfold[E, A](f: A => Option[(E, A)]): A => List[E] =
    new (A => List[E]) {
      kernel =>

      override def apply(a: A): List[E] =
        f(a) match {
          case Some((e, a)) => e :: kernel(a)
          case None => Nil
        }
    }

  val productOp: Option[(Int, Int)] => Int =
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
    println(
      foldRight[ListF[Int, *], List[Int], Int](productOp)(projectList)(patternFunctorInstance[Int])(1 :: 10 :: 20 :: Nil)
    )
    println(unfold(rangeOp)(10))
  }
}
