package combinators

import cats.Applicative
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.option._
import support.TestSupport

object ApplicativeExp
  extends TestSupport {
  def main(args: Array[String]): Unit = {
    //    def pure[A](x: A): F[A]
    //    def unit: F[Unit]
    //    def replicateA[A](n: Int, fa: F[A]): F[List[A]]
    //    def compose[G[_]: Applicative]: Applicative[λ[α => F[G[α]]]]
    //    def composeContravariantMonoidal[G[_]: ContravariantMonoidal]: ContravariantMonoidal[λ[α => F[G[α]]]]
    //    def unlessA[A](cond: Boolean)(f: => F[A]): F[Unit]
    //    def whenA[A](cond: Boolean)(f: => F[A]): F[Unit]

    1.pure[Option]
      .assertIs(1.some)

    1.pure[Option].replicateA(3)
      .assertIs(Some(List(1, 1, 1)))

    Applicative[Option]
      .compose[List]
      .pure(1)
      .assertIs(Some(List(1)))

    List(1, 2, 3)
      .unlessA(false)
      .assertIs(List((), (), ()))
    List(1, 2, 3)
      .unlessA(true)
      .assertIs(List(()))

    List(1, 2, 3)
      .whenA(false)
      .assertIs(List(()))
    List(1, 2, 3)
      .whenA(true)
      .assertIs(List((), (), ()))

    val inc: Int => (Int, Int) = i => (i, i + 1)
    Ref[IO].of(1)
      .flatMap(_.modify(inc))
      .unlessA(false)
      .unsafeRunSync()
      .assertIs(1)
    Ref[IO].of(1)
      .flatMap(_.modify(inc))
      .unlessA(true)
      .unsafeRunSync()
      .assertIs(2)

    Ref[IO].of(1)
      .flatMap(_.modify(inc))
      .whenA(false)
      .unsafeRunSync()
      .assertIs(2)
    Ref[IO].of(1)
      .flatMap(_.modify(inc))
      .whenA(true)
      .unsafeRunSync()
      .assertIs(1)

    ()
  }
}
