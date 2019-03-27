package combinators

import cats.FlatMap
import cats.effect.IO
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.option._
import support.TestSupport

object FlatMapExp
  extends TestSupport {
  def main(args: Array[String]): Unit = {
    //  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    //  def flatten[A](ffa: F[F[A]]): F[A]
    //  def mproduct[A, B](fa: F[A])(f: A => F[B]): F[(A, B)]
    //  def ifM[B](fa: F[Boolean])(ifTrue: => F[B], ifFalse: => F[B]): F[B]
    //  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
    //  def flatTap[A, B](fa: F[A])(f: A => F[B]): F[A]

    1.some
      .mproduct(i => if (i % 2 == 0) (i * 10).some else None)
      .shouldBe(None)
    2.some
      .mproduct(i => if (i % 2 == 0) (i * 10).some else None)
      .shouldBe((2, 20).some)

    FlatMap[Option]
      .ifM(true.some)(3.some, 4.some)
      .shouldBe(3.some)
    FlatMap[Option]
      .ifM(false.some)(3.some, 4.some)
      .shouldBe(4.some)

    1.some
      .flatTap(i => (i * 10).some)
      .shouldBe(1.some)
    Option.empty[Int]
      .flatTap(i => (i * 10).some)
      .shouldBe(Option.empty[Int])

    ////

    case class SomeInts(ints: List[Int])
    def readInt(n: Int): IO[Int] = IO(n)

    def readInts: IO[SomeInts] =
      FlatMap[IO]
        .tailRecM(SomeInts(Nil)) {
          case si if si.ints.size > 3 =>
            IO(Right(si))
          case si =>
            readInt(si.ints.length)
              .map(i => Left(SomeInts(si.ints :+ i)))
        }

    readInts
      .unsafeRunSync()
      .shouldBe(SomeInts(List(0, 1, 2, 3)))

    ()
  }
}
