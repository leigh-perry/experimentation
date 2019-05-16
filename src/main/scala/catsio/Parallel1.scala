package catsio

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.DurationInt
import scala.util.Random
import scala.util.control.NoStackTrace

object Parallel1 extends IOApp {
  final case class Fail(s: String) extends NoStackTrace

  def putStrLn[A](a: A) =
    for {
      i <- IO(Random.nextInt(10))
      _ <- IO.sleep((i * 10).millis)
      _ <- if (i % 2 == 0) IO.raiseError(Fail(s"Error=$i")) else IO(println(s"$i: $a"))
    } yield ()

  // Stop on first failure
  override def run(args: List[String]): IO[ExitCode] =
    (1 to 10).toList
      .parTraverse(putStrLn)
      .handleErrorWith {
        case Fail(s) => IO(println(s"First failure caught: $s")) *> IO(s)
      }
      .flatMap(s => IO(println(s"Outcome: $s")))
      .as(ExitCode.Success)
}
