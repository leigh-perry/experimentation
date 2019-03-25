package combinators

import cats.ApplicativeError
import cats.effect.IO
import cats.syntax.applicativeError._
import cats.syntax.either._
import testsupport.TestSupport

object ApplicativeErrorExp
  extends TestSupport {

  sealed trait ErrType extends Throwable
  final case object AnError extends ErrType

  def main(args: Array[String]): Unit = {
    //    def raiseError[A](e: E): F[A]
    //    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
    //    def handleError[A](fa: F[A])(f: E => A): F[A]
    //    def attempt[A](fa: F[A]): F[Either[E, A]]
    //    def attemptT[A](fa: F[A]): EitherT[F, E, A]
    //    def recover[A](fa: F[A])(pf: PartialFunction[E, A]): F[A]
    //    def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A]
    //    def onError[A](fa: F[A])(pf: PartialFunction[E, F[Unit]]): F[A]
    //    def catchNonFatal[A](a: => A)(implicit ev: Throwable <:< E): F[A]
    //    def catchNonFatalEval[A](a: Eval[A])(implicit ev: Throwable <:< E): F[A]
    //    def fromTry[A](t: Try[A])(implicit ev: Throwable <:< E): F[A]
    //    def fromEither[A](x: E Either A): F[A]

    AnError.raiseError[IO, Int]
      .handleErrorWith((_: Throwable) => IO(1234))
      .unsafeRunSync()
      .shouldBe(1234)

    AnError.raiseError[IO, Int]
      .handleError(_ => 1234)
      .unsafeRunSync()
      .shouldBe(1234)

    AnError.raiseError[IO, Int]
      .handleError(_ => 1234)
      .unsafeRunSync()
      .shouldBe(1234)

    AnError.raiseError[IO, Int]
      .attempt
      .unsafeRunSync()
      .shouldBe(AnError.asLeft)

    AnError.raiseError[IO, Int]
      .attemptT // EitherT
      .value
      .unsafeRunSync()
      .shouldBe(AnError.asLeft)

    AnError.raiseError[IO, Int]
      .recover {
        case AnError => 1234
      }
      .unsafeRunSync()
      .shouldBe(1234)

    AnError.raiseError[IO, Int]
      .recoverWith {
        case AnError => IO(1234)
      }
      .unsafeRunSync()
      .shouldBe(1234)

    AnError.raiseError[IO, Int]
      .onError {
        case AnError => IO.delay(println("(output on error)"))
      }
      .attempt
      .unsafeRunSync()
      .shouldBe(AnError.asLeft)

    ApplicativeError[IO, Throwable]
      .catchNonFatal[Int](throw AnError)
      .handleError(_ => 1234)
      .unsafeRunSync()
      .shouldBe(1234)
    ApplicativeError[IO, Throwable]
      .catchNonFatal[Int](throw AnError)
      .attempt
      .unsafeRunSync()
      .shouldBe(AnError.asLeft)

    ApplicativeError[IO, Throwable]
      .fromEither[Int](1234.asRight)
      .attempt
      .unsafeRunSync()
      .shouldBe(1234.asRight)
    ApplicativeError[IO, Throwable]
      .fromEither[Int](AnError.asLeft)
      .attempt
      .unsafeRunSync()
      .shouldBe(AnError.asLeft)

    ()
  }
}
