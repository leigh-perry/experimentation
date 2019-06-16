package exp

import cats.arrow.{Category, Profunctor}
import cats.data.ReaderT
import cats.effect.{Bracket, IO}
import cats.syntax.applicative._
import cats.syntax.profunctor._
import cats.effect.syntax.bracket._
import cats.{Applicative, Monad}


object LoggerStuff {
  final case class LogStage[A, B](
    debug: A => B,
    info: A => B,
    error: A => B
  )

  object LogStage {
    def unlevelledLogger[L, A](f: L => A): LogStage[L, A] =
      LogStage(f, f, f)

    implicit val loggerProfunctor: Profunctor[LogStage] =
      new Profunctor[LogStage] {
        override def dimap[A, B, C, D](fab: LogStage[A, B])(f: C => A)(g: B => D): LogStage[C, D] =
          new LogStage[C, D](
            g compose fab.debug compose f,
            g compose fab.info compose f,
            g compose fab.error compose f,
          )
      }

    implicit val loggerCategory: Category[LogStage] =
      new Category[LogStage] {
        override def id[A]: LogStage[A, A] =
          new LogStage[A, A](identity, identity, identity)

        override def compose[A, B, C](f: LogStage[B, C], g: LogStage[A, B]): LogStage[A, C] =
          new LogStage[A, C](
            f.debug compose g.debug,
            f.info compose g.info,
            f.error compose g.error,
          )
      }

    implicit def loggerMonad[A]: Monad[LogStage[A, ?]] =
      new Monad[LogStage[A, ?]] {
        override def pure[B](x: B): LogStage[A, B] =
          new LogStage[A, B](_ => x, _ => x, _ => x)

        override def flatMap[B, C](fa: LogStage[A, B])(f: B => LogStage[A, C]): LogStage[A, C] =
          new LogStage[A, C](
            a => f(fa.debug(a)).debug(a),
            a => f(fa.info(a)).info(a),
            a => f(fa.error(a)).error(a),
          )

        override def tailRecM[B, C](a: B)(f: B => LogStage[A, Either[B, C]]): LogStage[A, C] =
          ???
      }

    val levelledLogStage: LogStage[String, String] =
      new LogStage[String, String](
        s => s"DEBUG $s",
        s => s"INFO  $s",
        s => s"ERROR $s",
      )

    val consoleLogStage: LogStage[String, IO[Unit]] =
      levelledLogStage
        .rmap(s => IO.delay(println(s)))

    def logWithPlace(place: String, msg: String) =
      s"$place: $msg"

    val consoleLoggerWithPlace: LogStage[(String, String), IO[Unit]] =
      consoleLogStage
        .lmap((logWithPlace _).tupled)

    object Etc {
      def ask[F[_] : Applicative, A]: ReaderT[F, A, A] =
        ReaderT(_.pure[F])

      def setupRunSinkAndCleanup[F[_], R, I, O](
        setup: ReaderT[F, String, I],
        runBenchmarks: I => F[R],
        sink: R => F[O],
        cleanup: I => F[Unit],
      )(implicit F: Bracket[F, Throwable]): F[O] =
        for {
          start <- setup.local((s: String) => "setup> " + s)
          out <- (
            for {
              results <- ReaderT[F, String, String](runBenchmarks).local((v: String) => "run-benchmarks> " + v)
              output <- ReaderT[F, String, String](sink).local((v: String) => "output> " + v)
            } yield output
            ).guarantee(ReaderT[F, String, String](cleanup(start)).local((v: String) => "cleanup> " + v))
        } yield out
    }
  }
}
