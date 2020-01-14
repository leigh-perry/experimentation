package naive.io

final case class SyncE[E, A](unsafeRunEither: () => Either[E, A]) {
  def map[B](f: A => B): SyncE[E, B] =
    SyncE(
      () => unsafeRunEither().map(f)
    )

  def flatMap[B](f: A => SyncE[E, B]): SyncE[E, B] =
    SyncE(
      () =>
        for {
          a <- unsafeRunEither()
          b <- f(a).unsafeRunEither()
        } yield b
    )

  def attempt: SyncE[Nothing, Either[E, A]] =
    SyncE(
      () => Right(unsafeRunEither())
    )

  def foldM[B](failure: E => SyncE[E, B], success: A => SyncE[E, B]): SyncE[E, B] =
    SyncE
      .pure(())   // defer
      .flatMap(
        _ =>
          unsafeRunEither() match {
            case Left(e) => failure(e)
            case Right(a) => success(a)
          }
      )

  def unsafeRunSync(): A =
    unsafeRunEither() match {
      case Left(e) => throw new RuntimeException(e.toString)
      case Right(a) => a
    }
}

object SyncE {
  def pure[E, A](a: => A): SyncE[E, A] =
    SyncE(() => Right(a))

  def fail[E, A](e: E): SyncE[E, A] =
    SyncE(() => Left(e))

  def flatten[E, A](s: SyncE[E, SyncE[E, A]]): SyncE[E, A] =
    s.flatMap(identity)
}

////

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
