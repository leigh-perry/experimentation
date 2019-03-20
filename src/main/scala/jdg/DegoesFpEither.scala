package jdg

import cats.data.EitherT
import cats.effect.{IO, Sync}
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.{Applicative, Monad, MonadError}

import scala.util.{Failure, Success, Try}

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

  class ProductionInterpreter[F[_]](implicit S: Sync[F], E: MonadError[F, Error]) extends Algebra[F] {

    import scala.io.StdIn.readLine

    private def prompt(s: String): F[Unit] = S.delay(print(s"$s "))
    private def read: F[String] = S.delay(readLine())

    override def promptName: F[Unit] = prompt(s"Enter name:")
    override def readName: F[String] = read
    override def greet(name: String): F[Unit] = prompt(s"Hello $name")
    override def nextNumber: F[Int] =
      S.delay(scala.util.Random.nextInt(5) + 1)

    override def promptNumber(name: String): F[Unit] =
      prompt(s"Dear $name, please guess a number from 1 to 5:")

    override def readNumber: F[Int] =
      S.flatMap(read) {
        s =>
          Try(s.toInt) match {
            case Success(e) => S.pure(e)
            case Failure(_) => E.raiseError(Error.InvalidNumber(s))
          }
      }

    override def evaluate(target: Int, guessed: Int): F[Boolean] =
    //      S.pure(target == guessed)
      EvaluationSuite.program(new EvaluationSuite.ProductionInterpreter[F](S), target, guessed)(S)

    override def revealResult(name: String, target: Int, r: Boolean): F[Unit] =
      prompt(if (r) s"You guessed right, $name!" else s"You guessed wrong, $name! The number was: $target")

    override def promptContinue(name: String): F[Unit] = prompt("Do you want to continue, " + name + "?")
    override def readContinue: F[Boolean] =
      S.flatMap(read) {
        case "y" => S.pure(true)
        case "n" => S.pure(false)
        case s @ _ => E.raiseError(Error.InvalidChoice(s))
      }
  }

  ////

  object EvaluationSuite {

    import cats.Monad

    trait EvaluateAlgebra[F[_]] {
      def getTarget(b: Int): F[Int]
      def getGuess(b: Int): F[Int]
      def compareThem(target: Int, guess: Int): F[Boolean]
    }

    def program[F[_]](alg: EvaluateAlgebra[F], target: Int, guess: Int)(implicit M: Monad[F]): F[Boolean] =
      for {
        t <- alg.getTarget(target)
        g <- alg.getGuess(guess)
        r <- alg.compareThem(t, g)
      } yield r

    // Production version using IO
    class ProductionInterpreter[F[_]](/*implicit*/ M: Applicative[F]) extends EvaluateAlgebra[F] {
      override def getTarget(b: Int): F[Int] = M.pure(b)
      override def getGuess(b: Int): F[Int] = M.pure(b)
      override def compareThem(target: Int, guess: Int): F[Boolean] = M.pure(target == guess)
    }
  }

  ////

  private def pureMain(args: Array[String]) =
    program(new ProductionInterpreter[EitherT[IO, Error, ?]])
      .value

  def main(args: Array[String]): Unit = println(pureMain(args).unsafeRunSync())
}
