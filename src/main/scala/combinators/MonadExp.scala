package combinators

import cats.effect.IO
import cats.instances.vector._
import cats.syntax.monad._
import support.TestSupport

object MonadExp
  extends TestSupport {
  def main(args: Array[String]): Unit = {
    //    def whileM[G[_], A](p: F[Boolean])(body: => F[A])(implicit G: Alternative[G]): F[G[A]]
    //    def whileM_[A](p: F[Boolean])(body: => F[A]): F[Unit]
    //    def untilM[G[_], A](f: F[A])(cond: => F[Boolean])(implicit G: Alternative[G]): F[G[A]]
    //    def untilM_[A](f: F[A])(cond: => F[Boolean]): F[Unit]
    //    def iterateWhile [A](f: F[A])(p: A => Boolean): F[A]
    //    def iterateWhileM[A](init: A)(f: A => F[A])(p: A => Boolean): F[A]
    //    def iterateUntil[A](f: F[A])(p: A => Boolean): F[A]
    //    def iterateUntilM[A](init: A)(f: A => F[A])(p: A => Boolean): F[A]

    val runWhileTests = false
    if (runWhileTests) {
      val start = System.currentTimeMillis
      IO(1)
        .whileM[Vector](IO(System.currentTimeMillis < start + 500))
        .unsafeRunSync()
        .shouldSatisfy(v => v.nonEmpty && v.forall(_ == 1))
    }
    if (runWhileTests) {
      val start = System.currentTimeMillis
      IO.delay(print("do work "))
        .whileM_(IO(System.currentTimeMillis < start + 20))
        .unsafeRunSync()
        .shouldBe(())
    }

    // ditto untilM, untilM_

    if (true) {
      val start = System.currentTimeMillis
      IO.delay(System.currentTimeMillis)
        .iterateWhile(now => now < start + 20)
        .unsafeRunSync()
        .shouldSatisfy(finalValue => finalValue >= start + 20)
    }

    ////

    case class SomeInts(ints: List[Int])
    def readInt(n: Int): IO[Int] = IO(n)

    def readInts: IO[SomeInts] =
      SomeInts(Nil)
        .iterateUntilM(
          si =>
            readInt(si.ints.length).map(i => SomeInts(si.ints :+ i))
        )(_.ints.size > 3)

    readInts
      .unsafeRunSync()
      .shouldBe(SomeInts(List(0, 1, 2, 3)))

    ()
  }
}
