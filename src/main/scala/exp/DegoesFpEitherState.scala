package exp

import cats.data.{EitherT, StateT}
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.{Applicative, Monad}

import scala.util.Try

// For original imperative version see App0 in https://gist.github.com/jdegoes/1b43f43e2d1e845201de853815ab3cb9

object DegoesFpEitherState {

  trait Algebra[F[_]] {
    def promptName: F[Unit]
    def readName: F[String]
    def greet(name: String): F[Unit]
    def nextNumber: F[Int]
    def promptNumber(name: String): F[Unit]
    def readNumber: F[Int]
    def evaluate(target: Int, guessed: Int): F[Boolean]
    def revealResult(name: String, guessed: Int, r: Boolean): F[Unit]
    def promptContinue(name: String): F[Unit]
    def readContinue: F[Boolean]
  }

  def program[F[_] : Monad](alg: Algebra[F]): F[Boolean] =
    for {
      _ <- alg.promptName
      name <- alg.readName
      _ <- alg.greet(name)
      r <- loop(alg, name)
    } yield r

  def loop[F[_] : Monad](alg: Algebra[F], name: String): F[Boolean] =
    for {
      target <- alg.nextNumber
      _ <- alg.promptNumber(name)
      guessed <- alg.readNumber
      r <- alg.evaluate(target, guessed)
      _ <- alg.revealResult(name, target, r)
      _ <- alg.promptContinue(name)
      more <- alg.readContinue
      _ <- if (more) loop(alg, name) else Applicative[F].pure(())
    } yield r

  ////

  sealed trait Error
  object Error {
    final case class InvalidNumber(s: String) extends Error
    final case class InvalidChoice(s: String) extends Error
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  class ProductionInterpreter[F[_] : Sync] extends Algebra[StateT[EitherT[F, Error, ?], RNG, ?]] {

    import scala.io.StdIn.readLine

    type Result[T] = StateT[EitherT[F, Error, ?], RNG, T]

    private def prompt(s: String): Result[Unit] =
      StateT.liftF(EitherT.liftF[F, Error, Unit](Sync[F].delay(print(s"$s "))))
    private def read: Result[String] =
      StateT.liftF(EitherT.liftF(Sync[F].delay(readLine())))

    override def promptName: Result[Unit] = prompt(s"Enter name:")
    override def readName: Result[String] = read
    override def greet(name: String): Result[Unit] = prompt(s"Hello $name")
    override def nextNumber: Result[Int] =
      StateT(
        (s: RNG) =>
          EitherT.liftF(
            Sync[F].delay {
              val (i: Int, r2: RNG) = s.nextInt
              (r2, i % 5 + 1)
            }
          )
      )

    override def promptNumber(name: String): Result[Unit] =
      prompt(s"Dear $name, please guess a number from 1 to 5:")

    override def readNumber: Result[Int] =
      read.flatMap {
        s =>
          StateT.liftF(EitherT.fromOption(Try(s.toInt).toOption, Error.InvalidNumber(s)))
      }

    override def evaluate(target: Int, guessed: Int): Result[Boolean] = StateT.liftF(EitherT.rightT(target == guessed))
    override def revealResult(name: String, target: Int, r: Boolean): Result[Unit] =
      prompt(if (r) s"You guessed right, $name!" else s"You guessed wrong, $name! The number was: $target")

    override def promptContinue(name: String): Result[Unit] = prompt("Do you want to continue, " + name + "?")
    override def readContinue: Result[Boolean] =
      read.flatMap {
        case "y" => StateT.liftF(EitherT.rightT(true))
        case "n" => StateT.liftF(EitherT.rightT(false))
        case s @ _ => StateT.liftF(EitherT.leftT(Error.InvalidChoice(s)))
      }
  }

  private def pureMain(args: Array[String]): IO[Either[Error, (RNG, Boolean)]] =
    program(new ProductionInterpreter[IO])
      .run(SimpleRNG(0))
      .value

  def main(args: Array[String]): Unit = println(pureMain(args).unsafeRunSync())
}
