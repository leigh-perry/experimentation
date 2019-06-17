package exp

import cats.arrow.{Category, Profunctor}
import cats.data.{Kleisli, ReaderT}
import cats.effect.IO
import cats.syntax.applicative._
import cats.syntax.profunctor._
import cats.{Applicative, Monad}


object LoggerStage {
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
            debug = g compose fab.debug compose f,
            info = g compose fab.info compose f,
            error = g compose fab.error compose f,
          )
      }

    implicit val loggerCategory: Category[LogStage] =
      new Category[LogStage] {
        override def id[A]: LogStage[A, A] =
          new LogStage[A, A](
            debug = identity,
            info = identity,
            error = identity
          )

        override def compose[A, B, C](f: LogStage[B, C], g: LogStage[A, B]): LogStage[A, C] =
          new LogStage[A, C](
            debug = f.debug compose g.debug,
            info = f.info compose g.info,
            error = f.error compose g.error,
          )
      }

    implicit def loggerMonad[A]: Monad[LogStage[A, ?]] =
      new Monad[LogStage[A, ?]] {
        override def pure[B](x: B): LogStage[A, B] =
          new LogStage[A, B](
            debug = _ => x,
            info = _ => x,
            error = _ => x
          )

        override def flatMap[B, C](fa: LogStage[A, B])(f: B => LogStage[A, C]): LogStage[A, C] =
          new LogStage[A, C](
            debug = a => f(fa.debug(a)).debug(a),
            info = a => f(fa.info(a)).info(a),
            error = a => f(fa.error(a)).error(a),
          )

        override def tailRecM[B, C](a: B)(f: B => LogStage[A, Either[B, C]]): LogStage[A, C] =
          ???
      }

    val levelledLogStage: LogStage[String, String] =
      new LogStage[String, String](
        debug = s => s"DEBUG $s",
        info = s => s"INFO  $s",
        error = s => s"ERROR $s",
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

      //      def setupRunSinkAndCleanup[F[_], R, I, O](
      //        setup: ReaderT[F, String, I],
      //        runBenchmarks: I => F[R],
      //        sink: R => F[O],
      //        cleanup: I => F[Unit],
      //      )(implicit F: Bracket[F, Throwable]): F[O] =
      //        for {
      //          start <- setup.local((s: String) => "setup> " + s)
      //          out <- (
      //            for {
      //              results <- ReaderT[F, String, String](runBenchmarks).local((v: String) => "run-benchmarks> " + v)
      //              output <- ReaderT[F, String, String](sink).local((v: String) => "output> " + v)
      //            } yield output
      //            ).guarantee(ReaderT[F, String, String](cleanup(start)).local((v: String) => "cleanup> " + v))
      //        } yield out

      def convertLogFunction[F[_] : Monad, A, E, B](f: (E, A) => F[B]): A => ReaderT[F, E, B] =
        a => ReaderT[F, E, B](e => f(e, a))

      val readerLog: LogStage[String, ReaderT[IO, String, Unit]] =
        LogStage(
          debug = convertLogFunction((e, a) => consoleLoggerWithPlace.debug((e, a))),
          info = convertLogFunction((e, a) => consoleLoggerWithPlace.info((e, a))),
          error = convertLogFunction((e, a) => consoleLoggerWithPlace.error((e, a))),
        )

      final class LoggerAndScope[L, A](logger: LogStage[L, A], scope: String)

      trait HasLogger[S[_, _], L, A] {
        def getLogger(s: S[L, A]): LogStage[L, A]
        def modifyLogger(f: LogStage[L, A] => LogStage[L, A]): S[L, A] => S[L, A]
      }

      trait HasScope[S] {
        def getScope(s: S): String
        def modifyScope(f: String => String): S => S
      }

      //  : MonadBracket : MonadReader[?[_], Env]

      //      trait MonadReader[F[_], S] extends Monad[F] { self =>
      //        def ask: F[S]
      //        def local[A](f: S => S)(fa: F[A]): F[A]

      //      def local[S](f: S => S): Kleisli[F, S, S] =
      //        Kleisli(f.andThen(run))

      //      def setupRunSinkAndCleanup[F[_], Env, R, I, O, S](
      //        setup: ReaderT[F, S, S],
      //        runBenchmarks: I => F[R],
      //        sink: R => F[O],
      //        cleanup: I => F[Unit]
      //      )(implicit S: HasScope[S]): F[O] =
      //        for {
      //          start <- setup.local(value => value.modifyScope(value1 => "Setup->" + value1))
      //          out <- (for {
      //            results <- runBenchmarks(start).local(S.modifyScope("Run->" + _))
      //            output <- sink(results).local(S.modifyScope("Output->" + _))
      //          }).guarantee(cleanup(start)).local(S.modifyScope("Cleanup->" + _))
      //        } yield out

    }
  }

  ////

  def main(args: Array[String]): Unit = {

    val cfg = Config(2)

    (new Ops)
      .doStuff
      .run(cfg)
      .unsafeRunSync()
  }

  final case class Config(n: Int)

  class Ops {
    def doStuff[S]: Kleisli[IO, Config, Unit] =
      Kleisli(cfg => IO.delay(println(s"stuff done: $cfg")))
  }
}
