package naive

sealed class SyncE[E, A](val unsafeRunEither: () => Either[E, A]) {
  def map[B](f: A => B): SyncE[E, B] =
    flatMap(a => SyncE.pure(f(a)))

  def flatMap[B](f: A => SyncE[E, B]): SyncE[E, B] =
    new SyncE(
      () =>
        for {
          a <- unsafeRunEither()
          b <- f(a).unsafeRunEither()
        } yield b
    )

  def flatten(s: SyncE[E, SyncE[E, A]]): SyncE[E, A] =
    s.flatMap(identity)

  def attempt: SyncE[Nothing, Either[E, A]] =
    new SyncE(
      () => Right(unsafeRunEither())
    )

  def foldM[B](failure: E => SyncE[E, B], success: A => SyncE[E, B]): SyncE[E, B] =
    SyncE.pure(()).flatMap {
      _ =>
        unsafeRunEither() match {
          case Left(e) => failure(e)
          case Right(a) => success(a)
        }
    }

  def unsafeRunSync(): A =
    unsafeRunEither() match {
      case Left(e) => throw new RuntimeException(e.toString)
      case Right(a) => a
    }
}

object SyncE {
  def pure[E, A](a: => A): SyncE[E, A] =
    new SyncE(() => Right(a))

  def fail[E, A](e: E): SyncE[E, A] =
    new SyncE(() => Left(e))
}

object SyncEApp {
  trait AppError
  final case class AppFail(error: String) extends AppError

  def main(args: Array[String]): Unit = {
    val programSuccess =
      for {
        a <- SyncE.pure[AppError, String]("a")
        b <- SyncE.pure[AppError, String]("b")
      } yield a + b

    val foldMedSuccess =
      programSuccess.foldM[Boolean](
        _ => SyncE.pure(false),
        _ => SyncE.pure(true)
      )

    println(programSuccess.unsafeRunSync())
    println(foldMedSuccess.unsafeRunSync())
    println(programSuccess.attempt.unsafeRunSync())

    val programFailure =
      for {
        a <- SyncE.pure[AppError, String]("a")
        b <- SyncE.fail[AppError, String](AppFail("oops"))
      } yield a + b

    val foldMedFailure =
      programFailure.foldM[Boolean](
        _ => SyncE.pure(false),
        _ => SyncE.pure(true)
      )

    println(foldMedFailure.unsafeRunSync())
    println(programFailure.attempt.unsafeRunSync())
  }
}
