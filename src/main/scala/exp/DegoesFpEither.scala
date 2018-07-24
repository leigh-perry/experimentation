package exp

import cats.data.EitherT
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.{Applicative, Monad}

import scala.util.Try

// For original imperative version see App0 in https://gist.github.com/jdegoes/1b43f43e2d1e845201de853815ab3cb9

object DegoesFpEither {

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

  class ProductionInterpreter[F[_] : Sync] extends Algebra[EitherT[F, Error, ?]] {

    import scala.io.StdIn.readLine

    type Result[T] = EitherT[F, Error, T]

    private def prompt(s: String): Result[Unit] = EitherT.liftF(Sync[F].delay(print(s"$s ")))
    private def read: Result[String] = EitherT.liftF(Sync[F].delay(readLine()))

    override def promptName: Result[Unit] = prompt(s"Enter name:")
    override def readName: Result[String] = read
    override def greet(name: String): Result[Unit] = prompt(s"Hello $name")
    override def nextNumber: Result[Int] =
      EitherT.liftF(Sync[F].delay(scala.util.Random.nextInt(5) + 1))

    override def promptNumber(name: String): Result[Unit] =
      prompt(s"Dear $name, please guess a number from 1 to 5:")

    override def readNumber: Result[Int] =
      read.flatMap(s => EitherT.fromOption(Try(s.toInt).toOption, Error.InvalidNumber(s)))

    override def evaluate(target: Int, guessed: Int): Result[Boolean] = EitherT.rightT(target == guessed)
    override def revealResult(name: String, target: Int, r: Boolean): Result[Unit] =
      prompt(if (r) s"You guessed right, $name!" else s"You guessed wrong, $name! The number was: $target")

    override def promptContinue(name: String): Result[Unit] = prompt("Do you want to continue, " + name + "?")
    override def readContinue: Result[Boolean] =
      read.flatMap {
        case "y" => EitherT.rightT(true)
        case "n" => EitherT.rightT(false)
        case s @ _ => EitherT.leftT(Error.InvalidChoice(s))
      }
  }

  private def pureMain(args: Array[String]): IO[Either[Error, Boolean]] =
    program(new ProductionInterpreter[IO])
      .value

  def main(args: Array[String]): Unit = println(pureMain(args).unsafeRunSync())
}
