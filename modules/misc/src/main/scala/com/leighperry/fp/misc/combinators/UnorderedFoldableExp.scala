package combinators

import cats.UnorderedFoldable
import cats.instances.int._
import cats.instances.list._
import cats.syntax.unorderedFoldable._
import support.TestSupport

object UnorderedFoldableExp
  extends TestSupport {

  def main(args: Array[String]): Unit = {

    //    def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B
    //    def unorderedFold[A: CommutativeMonoid](fa: F[A]): A
    //    def isEmpty[A](fa: F[A]): Boolean
    //    def nonEmpty[A](fa: F[A]): Boolean
    //    def exists[A](fa: F[A])(p: A => Boolean): Boolean
    //    def forall[A](fa: F[A])(p: A => Boolean): Boolean
    //    def size[A](fa: F[A]): Long

    val uf: UnorderedFoldable[List] = UnorderedFoldable[List]

    List(1, 2, 3)
      .unorderedFoldMap(_ * 10)
      .assertIs(60)

    List(1, 2, 3)
      .unorderedFold
      .assertIs(6)

    uf.isEmpty(List())
      .assertIs(true)

    uf.nonEmpty(List(1))
      .assertIs(true)

    uf.exists(List(1, 2, 3))(_ == 2)
      .assertIs(true)

    uf.forall(List(1, 2, 3))(_ > 0)
      .assertIs(true)

    uf.size(List(1, 2, 3))
      .assertIs(3)

    ()
  }
}
