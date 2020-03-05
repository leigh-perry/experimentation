package naive.io

final case class SyncRE[R, E, A](unsafeRunEither: R => Either[E, A]) {
  def map[B](f: A => B): SyncRE[R, E, B] =
    SyncRE(
      env => unsafeRunEither(env).map(f)
    )

  def flatMap[B](f: A => SyncRE[R, E, B]): SyncRE[R, E, B] =
    SyncRE(
      (env: R) =>
        for {
          a <- unsafeRunEither(env)
          b <- f(a).unsafeRunEither(env)
        } yield b
    )

  def attempt: SyncRE[R, Nothing, Either[E, A]] =
    SyncRE(
      (env: R) => Right(unsafeRunEither(env))
    )

  def foldM[B](failure: E => SyncRE[R, E, B], success: A => SyncRE[R, E, B]): SyncRE[R, E, B] =
    SyncRE {
      (env: R) =>
        unsafeRunEither(env) match {
          case Left(e) =>
            failure(e).unsafeRunEither(env)
          case Right(a) =>
            success(a).unsafeRunEither(env)
        }
    }

  def provide(r: R): SyncRE[Any, E, A] =
    SyncRE(
      _ => unsafeRunEither(r)
    )
}

object SyncRE {
  def pure[R, E, A](a: => A): SyncRE[R, E, A] =
    SyncRE((_: R) => Right(a))

  def fail[R, E, A](e: E): SyncRE[R, E, A] =
    SyncRE((_: R) => Left(e))

  def accessM[R, E, A](f: R => SyncRE[R, E, A]): SyncRE[R, E, A] =
    SyncRE {
      r =>
        f(r).unsafeRunEither(r)
    }

  def flatten[R, E, A](s: SyncRE[R, E, SyncRE[R, E, A]]): SyncRE[R, E, A] =
    s.flatMap(identity)

  def unsafeRunSyncAny[E, A](io: SyncRE[Any, E, A]): A =
    io.unsafeRunEither(()) match {
      case Left(e) => throw new RuntimeException(e.toString)
      case Right(a) => a
    }

  implicit class Syntax[E, A](io: SyncRE[Any, E, A]) {
    def unsafeRunSync(): A =
      SyncRE.unsafeRunSyncAny(io)
  }

  implicit class SyntaxNothing[A](io: SyncRE[Any, Nothing, A]) {
    def unsafeRunSync(): A =
      SyncRE.unsafeRunSyncAny(io)
  }
}

////

object SyncREApp {
  trait AppError
  final case class AppFail(error: String) extends AppError

  def main(args: Array[String]): Unit = {
    val env = 1234L

    val programSuccessR: SyncRE[Long, AppError, String] =
      SyncRE.accessM(
        env =>
          for {
            a <- SyncRE.pure[Long, AppError, String]("a")
            b <- SyncRE.pure[Long, AppError, String]("b")
          } yield a + b + env
      )
    val programSuccess: SyncRE[Any, AppError, String] = programSuccessR.provide(env)
    val foldMSuccess =
      programSuccess.foldM[Boolean](
        _ => SyncRE.pure(false),
        _ => SyncRE.pure(true)
      )

    println(programSuccess.unsafeRunSync())
    println(foldMSuccess.unsafeRunSync())
    println(programSuccessR.attempt.provide(env).unsafeRunSync())

    val programFailureR: SyncRE[Long, AppError, String] =
      for {
        a <- SyncRE.pure[Long, AppError, String]("a")
        b <- SyncRE.fail[Long, AppError, String](AppFail("oops" + env))
      } yield a + b + env

    val programFailure: SyncRE[Any, AppError, String] = programFailureR.provide(env)
    val foldMFailure =
      programFailure.foldM[Boolean](
        _ => SyncRE.pure(false),
        _ => SyncRE.pure(true)
      )

    println(foldMFailure.unsafeRunSync())
    println(programFailure.attempt.unsafeRunSync())
  }
}
