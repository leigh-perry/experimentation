package catsio

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.DurationInt
import scala.util.Random
import scala.util.control.NoStackTrace

object Parallel3 extends IOApp {
  final case class Fail(s: String) extends NoStackTrace

  def putStrLn[A](a: A): IO[Unit] =
    for {
      i <- IO(Random.nextInt(10))
      _ <- IO.sleep((i * 10).millis)
      _ <- if (i % 2 == 0) IO.raiseError(Fail(s"error $i")) else IO(println(s"$i: $a"))
    } yield ()

  // Stop let all complete and gather all outcomes
  override def run(args: List[String]): IO[ExitCode] =
    (1 to 10).toList
      .parTraverse {
        i =>
          Deferred[IO, Either[Throwable, Unit]]
            .flatMap {
              deferred =>
                putStrLn(i)
                  .recoverWith {
                    case e: Fail => IO(println(e)) *> IO.raiseError(e)
                  }
                  .attempt
                  .flatMap(deferred.complete)
                  .start *>
                  deferred.get.rethrow
            }
      }
      .handleErrorWith {
        case Fail(s) => IO(println(s"First failure caught: $s")) *> IO(s)
      }
      .flatMap(s => IO(println(s"Outcome: $s")))
      .as(ExitCode.Success)
}
