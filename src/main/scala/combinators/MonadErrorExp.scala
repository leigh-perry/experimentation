package combinators

import cats.effect.IO
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.monadError._
import testsupport.TestSupport

object MonadErrorExp
  extends TestSupport {

  sealed trait ErrType extends Throwable
  final case object AnError extends ErrType
  final case object AnotherError extends ErrType
  final case object YetAnotherError extends ErrType

  def main(args: Array[String]): Unit = {
    //    def ensure[A]  (fa: F[A])(error: => E)(predicate: A => Boolean): F[A]
    //    def ensureOr[A](fa: F[A])(error: A => E)(predicate: A => Boolean): F[A]
    //    def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A]
    //    def rethrow[A](fa: F[Either[E, A]]): F[A]

    IO(1234)
      .ensure(AnError)(_ == 1234)
      .unsafeRunSync()
      .shouldBe(1234)
    IO(123)
      .ensure(AnError)(_ == 1234)
      .attempt
      .unsafeRunSync()
      .shouldBe(AnError.asLeft)

    IO(1234)
      .ensureOr(i => if (i == 123) AnError else AnotherError)(_ == 1234)
      .unsafeRunSync()
      .shouldBe(1234)
    IO(123)
      .ensureOr(i => if (i == 123) AnError else AnotherError)(_ == 1234)
      .attempt
      .unsafeRunSync()
      .shouldBe(AnError.asLeft)
    IO(0)
      .ensureOr(i => if (i == 123) AnError else AnotherError)(_ == 1234)
      .attempt
      .unsafeRunSync()
      .shouldBe(AnotherError.asLeft)

    AnError.raiseError[IO, Int]
      .adaptError {
        case AnError => AnotherError
        case AnotherError => YetAnotherError
      }
      .attempt
      .unsafeRunSync()
      .shouldBe(AnotherError.asLeft)
    AnotherError.raiseError[IO, Int]
      .adaptError {
        case AnError => AnotherError
        case AnotherError => YetAnotherError
      }
      .attempt
      .unsafeRunSync()
      .shouldBe(YetAnotherError.asLeft)

    AnotherError.raiseError[IO, Int]
      .attempt
      .rethrow
      .attempt
      .unsafeRunSync()
      .shouldBe(AnotherError.asLeft)

    ()
  }
}
