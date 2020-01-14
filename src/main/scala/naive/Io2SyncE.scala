package naive

sealed class SyncE[E, A](val unsafeRunSync: () => Either[E, A]) {
  def map[B](f: A => B): SyncE[E, B] =
    flatMap(a => SyncE.pure(f(a)))

  def flatMap[B](f: A => SyncE[E, B]): SyncE[E, B] =
    new SyncE(
      () =>
        for {
          a <- unsafeRunSync()
          b <- f(a).unsafeRunSync()
        } yield b
    )

  def attempt: SyncE[Nothing, Either[E, A]] =
    new SyncE(
      () => Right(unsafeRunSync())
    )
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

    println(programSuccess.unsafeRunSync())
    println(programSuccess.attempt.unsafeRunSync())

    val programFailure =
      for {
        a <- SyncE.pure[AppError, String]("a")
        b <- SyncE.fail[AppError, String](AppFail("oops"))
      } yield a + b

    println(programFailure.unsafeRunSync())
    println(programFailure.attempt.unsafeRunSync())
  }
}
