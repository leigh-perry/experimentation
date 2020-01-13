package naive

sealed class Sync[A](val unsafePerformSync: () => A) {
  final def map[B](f: A => B): Sync[B] =
    new Sync(() => f(unsafePerformSync()))

  final def flatMap[B](f: A => Sync[B]): Sync[B] =
    new Sync(
      () =>
        f(unsafePerformSync())
          .unsafePerformSync()
    )

  final def attempt: Sync[Either[Throwable, A]] =
    new Sync(
      () =>
        try Right(unsafePerformSync())
        catch {
          case t: Throwable =>
            Left(t)
        }
    )
}

object Sync {
  final def apply[A](a: => A): Sync[A] =
    new Sync(() => a)

  final def fail[A](t: Throwable): Sync[A] =
    new Sync(() => throw t)
}

object IO_1_App {
  def main(args: Array[String]): Unit = {
    val programSuccess =
      for {
        a <- Sync("a")
        b <- Sync("b")
      } yield a + b

    println(programSuccess.attempt.unsafePerformSync())

    val programFailure =
      for {
        a <- Sync("a")
        b <- Sync.fail(new RuntimeException("oops"))
      } yield a + b

    println(programFailure.attempt.unsafePerformSync())
  }
}
