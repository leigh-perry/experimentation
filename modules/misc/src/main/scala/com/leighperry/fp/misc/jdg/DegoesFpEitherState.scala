package jdg

import cats.data.{EitherT, StateT}
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect.{IO, Sync}
import cats.{Applicative, Monad, MonadError}

import scala.util.{Failure, Success, Try}

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


  class ProductionInterpreter[F[_]](implicit S: Sync[F], E: MonadError[F, Error]) extends Algebra[StateT[F, RNG, ?]] {

    import scala.io.StdIn.readLine

    type Result[T] = StateT[F, RNG, T]
    private def liftF[T](f: F[T]): Result[T] = StateT.liftF(f)(S) // ambiguous implicits
    private def result[T](t: T): Result[T] = liftF(S.pure(t))

    private def writePrompt(s: String): F[Unit] = S.delay(print(s"$s "))
    private def readInput: F[String] = S.delay(readLine())

    private def prompt(s: String): Result[Unit] = liftF(writePrompt(s))
    private def read: Result[String] = liftF(readInput)

    override def promptName: Result[Unit] = prompt(s"Enter name:")
    override def readName: Result[String] = read
    override def greet(name: String): Result[Unit] = prompt(s"Hello $name")
    override def nextNumber: Result[Int] =
      StateT(
        (s: RNG) =>
          S.delay {
            val (i: Int, r2: RNG) = s.nextInt
            (r2, i % 5 + 1)
          }
      )(S)

    override def promptNumber(name: String): Result[Unit] =
      prompt(s"Dear $name, please guess a number from 1 to 5:")

    override def readNumber: Result[Int] =
      liftF(
        S.flatMap[String, Int](readInput) {
          s =>
            Try(s.toInt) match {
              case Success(e) => S.pure(e)
              case Failure(_) => E.raiseError(Error.InvalidNumber(s))
            }
        }
      )

    override def evaluate(target: Int, guessed: Int): Result[Boolean] = result(target == guessed)
    override def revealResult(name: String, target: Int, r: Boolean): Result[Unit] =
      prompt(if (r) s"You guessed right, $name!" else s"You guessed wrong, $name! The number was: $target")

    override def promptContinue(name: String): Result[Unit] = prompt("Do you want to continue, " + name + "?")
    override def readContinue: Result[Boolean] =
      liftF(
        S.flatMap[String, Boolean](readInput) {
          case "y" => S.pure(true)
          case "n" => S.pure(false)
          case s @ _ => E.raiseError(Error.InvalidChoice(s))
        }
      )
  }

  private def pureMain(args: Array[String]): IO[Either[Error, (RNG, Boolean)]] =
    program(new ProductionInterpreter[EitherT[IO, Error, ?]])
      .run(SimpleRNG(0))
      .value

  def main(args: Array[String]): Unit = println(pureMain(args).unsafeRunSync())
}
