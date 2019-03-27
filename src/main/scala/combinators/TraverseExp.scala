package combinators

import cats.Traverse
import cats.instances.list._
import cats.instances.option._
import cats.syntax.option._
import cats.syntax.traverse._
import support.{OptionalSupport, TestSupport}

object TraverseExp
  extends TestSupport
    with OptionalSupport {

  def main(args: Array[String]): Unit = {

    //    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    //    def flatTraverse[G[_], A, B]\(fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]
    //    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]
    //    def flatSequence[G[_], A](fgfa: F[G[F[A]]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[A]
    //    def compose[G[_]: Traverse]: Traverse[λ[α => F[G[α]]]
    //    def mapWithIndex[A, B](fa: F[A])(f: (A, Int) => B): F[B
    //    def traverseWithIndexM[G[_], A, B](fa: F[A])(f: (A, Int) => G[B])(implicit G: Monad[G]): G[F[B]
    //    def zipWithIndex[A](fa: F[A]): F[(A, Int)] =

    List(1, 2, 3)
      .traverse(_.some)
      .shouldBe(List(1, 2, 3).some)
    List(1, 2, 3)
      .traverse(i => (i != 2).option(i))
      .shouldBe(None)

    // def traverse    [G[_], A, B](fa: F[A])(f: A => G[B]   ): G[F[B]]
    // def flatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]]): G[F[B]]
    List(1, 2, 3) // G = Option, F = List
      .flatTraverse(i => Option(List(i, i * 10)))
      .shouldBe(List(1, 10, 2, 20, 3, 30).some)

    List(1.some, 2.some, 3.some)
      .sequence
      .shouldBe(List(1, 2, 3).some)
    List(1.some, 2.some, None)
      .sequence
      .shouldBe(None)

    // def sequence    [G[_], A](fga:  F[G[A]]   ): G[F[A]
    // def flatSequence[G[_], A](fgfa: F[G[F[A]]]): G[F[A]
    List(List(1, 2, 3).some) // G = Option, F = List
      .flatSequence[Option, Int]
      .shouldBe(List(1, 2, 3).some)
    List(Option.empty[List[Int]]) // G = Option, F = List
      .flatSequence[Option, Int]
      .shouldBe(None)

    List(1, 2, 3)
      .mapWithIndex((a, i) => (i, a).toString)
      .shouldBe(List("(0,1)", "(1,2)", "(2,3)"))

    // def mapWithIndex      [A, B]      (fa: F[A])(f: (A, Int) => B   ): F[B
    // def traverseWithIndexM[G[_], A, B](fa: F[A])(f: (A, Int) => G[B]): G[F[B]
    List(1, 2, 3)
      .traverseWithIndexM((a, i) => (i, a).toString.some)
      .shouldBe(List("(0,1)", "(1,2)", "(2,3)").some)

    Traverse[List]
      .zipWithIndex(List(1, 2, 3))
      .shouldBe(List((1, 0), (2, 1), (3, 2)))

    ()
  }
}
