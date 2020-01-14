package naive

final case class Sync[A](unsafeRunSync: () => A) {
  def map[B](f: A => B): Sync[B] =
    Sync(
      () => f(unsafeRunSync())
    )

  def flatMap[B](f: A => Sync[B]): Sync[B] =
    Sync(
      () =>
        f(unsafeRunSync())
          .unsafeRunSync()
    )

  def attempt: Sync[Either[Throwable, A]] =
    Sync(
      () =>
        try Right(unsafeRunSync())
        catch {
          case t: Throwable =>
            Left(t)
        }
    )
}

object Sync {
  def pure[A](a: => A): Sync[A] =
    Sync(() => a)

  def fail[A](t: Throwable): Sync[A] =
    Sync(() => throw t)

  def flatten[A](s: Sync[Sync[A]]): Sync[A] =
    s.flatMap(identity)
}

////

object SyncApp {
  def main(args: Array[String]): Unit = {
    val programSuccess =
      for {
        a <- Sync.pure("a")
        b <- Sync.pure("b")
      } yield a + b

    println(programSuccess.attempt.unsafeRunSync())

    val programFailure =
      for {
        a <- Sync.pure("a")
        b <- Sync.fail(new RuntimeException("oops"))
      } yield a + b

    println(programFailure.attempt.unsafeRunSync())
  }
}
