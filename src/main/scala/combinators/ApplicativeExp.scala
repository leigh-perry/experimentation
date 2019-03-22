package combinators

import cats.Applicative
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.option._
import testsupport.TestSupport

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
      .shouldBe(1.some)

    1.pure[Option].replicateA(3)
      .shouldBe(Some(List(1, 1, 1)))

    Applicative[Option]
      .compose[List]
      .pure(1)
      .shouldBe(Some(List(1)))

    List(1, 2, 3)
      .unlessA(false)
      .shouldBe(List((), (), ()))
    List(1, 2, 3)
      .unlessA(true)
      .shouldBe(List(()))

    List(1, 2, 3)
      .whenA(false)
      .shouldBe(List(()))
    List(1, 2, 3)
      .whenA(true)
      .shouldBe(List((), (), ()))

    Ref[IO].of(1)
      .flatMap(_.modify(i => (i, i + 1)))
      .unlessA(false)
      .unsafeRunSync()
      .shouldBe(1)
    Ref[IO].of(1)
      .flatMap(_.modify(i => (i, i + 1)))
      .unlessA(true)
      .unsafeRunSync()
      .shouldBe(2)

    Ref[IO].of(1)
      .flatMap(_.modify(i => (i, i + 1)))
      .whenA(false)
      .unsafeRunSync()
      .shouldBe(2)
    Ref[IO].of(1)
      .flatMap(_.modify(i => (i, i + 1)))
      .whenA(true)
      .unsafeRunSync()
      .shouldBe(1)

    ()
  }
}
