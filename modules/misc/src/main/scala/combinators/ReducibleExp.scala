package combinators

import cats.Reducible
import cats.data.{Ior, NonEmptyList}
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.option._
import cats.syntax.reducible._
import support.TestSupport

object ReducibleExp
  extends TestSupport {
  def main(args: Array[String]): Unit = {

    //    def reduceLeft[A](fa: F[A])(f: (A, A) => A): A
    //    def reduceRight[A](fa: F[A])(f: (A, Eval[A]) => Eval[A]): Eval[A]
    //    def reduce[A](fa: F[A])(implicit A: Semigroup[A]): A
    //    def reduceK[G[_], A](fga: F[G[A]])(implicit G: SemigroupK[G]): G[A]
    //    def reduceMap[A, B](fa: F[A])(f: A => B)(implicit B: Semigroup[B]): B
    //    def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B
    //    def reduceLeftM[G[_], A, B](fa: F[A])(f: A => G[B])(g: (B, A) => G[B])(implicit G: FlatMap[G]): G[B]
    //    def reduceMapM[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: FlatMap[G], B: Semigroup[B]): G[B]
    //    def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B]
    //    def nonEmptyTraverse_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Apply[G]): G[Unit]
    //    def nonEmptySequence_[G[_], A](fga: F[G[A]])(implicit G: Apply[G]): G[Unit]
    //    def toNonEmptyList[A](fa: F[A]): NonEmptyList[A]
    //    def compose[G[_]: Reducible]: Reducible[λ[α => F[G[α]]]]
    //    def minimum[A](fa: F[A])(implicit A: Order[A]): A
    //    def maximum[A](fa: F[A])(implicit A: Order[A]): A
    //    def nonEmptyIntercalate[A](fa: F[A], a: A)(implicit A: Semigroup[A]): A
    //    def nonEmptyPartition[A, B, C](fa: F[A])(f: A => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]]

    NonEmptyList.of(1, 2, 3)
      .reduceLeft(_ + _)
      .assertIs(6)

    Reducible[NonEmptyList]
      .reduceRight(NonEmptyList.of(1, 2, 3))((value1, value2) => value2.map(_ + value1))
      .value
      .assertIs(6)

    NonEmptyList.of(1, 2, 3)
      .reduce
      .assertIs(6)

    NonEmptyList.of(1.some, 2.some, 3.some)
      .reduceK
      .assertIs(1.some)
    NonEmptyList.of(None, 2.some, 3.some)
      .reduceK
      .assertIs(2.some)
    NonEmptyList.of(None, None, 3.some)
      .reduceK
      .assertIs(3.some)
    NonEmptyList.of(None, None, None)
      .reduceK
      .assertIs(None)

    NonEmptyList.of(1, 2, 3)
      .reduceMap(_ * 2)
      .assertIs(6 * 2)

    Reducible[NonEmptyList]
      .reduceRightTo(NonEmptyList.of(1, 2, 3))(_ * 2)((value1, value2) => value2.map(_ + value1))
      .value
      .assertIs(3 * 2 + 2 + 1)

    NonEmptyList.of(1, 2, 3)
      .nonEmptyTraverse_(_.some)
      .assertIs(().some)

    NonEmptyList.of(1, 2, 3)
      .toNonEmptyList
      .assertIs(NonEmptyList.of(1, 2, 3))

    NonEmptyList.of(1, 2, 3)
      .minimum
      .assertIs(1)
    NonEmptyList.of(1, 2, 3)
      .maximum
      .assertIs(3)

    NonEmptyList.of("1", "2", "3")
      .nonEmptyIntercalate(".")
      .assertIs("1.2.3")
    NonEmptyList.of(1, 2, 3)
      .nonEmptyIntercalate(10)
      .assertIs(1 + 2 + 3 + 2 * 10)

    NonEmptyList.of(1, 2, 3)
      .nonEmptyPartition(i => Either.cond(i == 2, i, i))
      .assertIs(Ior.Both(NonEmptyList.of(1, 3), NonEmptyList.of(2)))

    ()
  }
}
