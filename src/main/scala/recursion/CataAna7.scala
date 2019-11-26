package recursion

import cats.Functor
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.functor._

object CataAna7 {
  def foldRight[E, B](f: Option[(E, B)] => B)(project: List[E] => Option[(E, List[E])]): List[E] => B = {
    type ListF[P] = Option[(E, P)]
    type S = List[E]

    implicit val patternFunctorInstance: Functor[ListF] =
      Functor[Option].compose[(E, *)]

    new (S => B) {
      kernel =>

      def step2: ListF[S] => ListF[B] =
        _.fmap(kernel)

      def step3: ListF[B] => B =
        f

      override def apply(list: S): B =
        step3(step2(project(list)))
    }
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

  // impl here is last bit specific to List
  def projectList[E]: List[E] => Option[(E, List[E])] =
    _ match {
      case Nil => Option.empty[(E, List[E])]
      case ::(head, tl) => Option((head, tl))
    }

  val rangeOp: Int => Option[(Int, Int)] =
    x => if (x > 0) Some((x, x - 1)) else None

  def main(args: Array[String]): Unit = {
    println(foldRight(productOp)(projectList)(1 :: 10 :: 20 :: Nil))
    println(unfold(rangeOp)(10))
  }
}
