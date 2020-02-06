package jdg

import cats.data.EitherT
import cats.effect.{IO, Sync}
import cats.free.Free
import cats.{MonadError, ~>}

import scala.util.{Failure, Success, Try}

object DegoesFpEitherFree {

  sealed trait Algebra[A]

  object Algebra {
    final case object PromptName extends Algebra[Unit]
    final case object ReadName extends Algebra[String]
    final case class Greet(name: String) extends Algebra[Unit]
    final case object NextNumber extends Algebra[Int]
    final case class PromptNumber(name: String) extends Algebra[Unit]
    final case object ReadNumber extends Algebra[Int]
    final case class Evaluate(target: Int, guessed: Int) extends Algebra[Boolean]
    final case class RevealResult(name: String, guessed: Int, r: Boolean) extends Algebra[Unit]
    final case class PromptContinue(name: String) extends Algebra[Unit]
    final case object ReadContinue extends Algebra[Boolean]
    final case object Finish extends Algebra[Unit]
  }

  def program: Free[Algebra, Boolean] =
    for {
      _ <- Free.liftF(Algebra.PromptName)
      name <- Free.liftF(Algebra.ReadName)
      _ <- Free.liftF(Algebra.Greet(name))
      r <- loop(name)
    } yield r

  def loop(name: String): Free[Algebra, Boolean] =
    for {
      target <- Free.liftF(Algebra.NextNumber)
      _ <- Free.liftF(Algebra.PromptNumber(name))
      guessed <- Free.liftF(Algebra.ReadNumber)
      r <- Free.liftF(Algebra.Evaluate(target, guessed))
      _ <- Free.liftF(Algebra.RevealResult(name, target, r))
      _ <- Free.liftF(Algebra.PromptContinue(name))
      more <- Free.liftF(Algebra.ReadContinue)
      _ <- if (more) loop(name) else Free.liftF[Algebra, Unit](Algebra.Finish)
    } yield r


  sealed trait Error
  object Error {
    final case class InvalidNumber(s: String) extends Error
    final case class InvalidChoice(s: String) extends Error
  }

  private def productionInterpreter[F[_]](implicit S: Sync[F], E: MonadError[F, Error]): Algebra ~> F =
    new (Algebra ~> F) {

      import scala.io.StdIn.readLine

      private def prompt(s: String): F[Unit] = S.delay(print(s"$s "))
      private def read: F[String] = S.delay(readLine())

      override def apply[A](fa: Algebra[A]): F[A] =
        fa match {
          case Algebra.PromptName => prompt(s"Enter name:")
          case Algebra.ReadName => read
          case Algebra.Greet(name: String) => prompt(s"Hello $name")
          case Algebra.NextNumber => S.delay(scala.util.Random.nextInt(5) + 1)
          case Algebra.PromptNumber(name: String) => prompt(s"Dear $name, please guess a number from 1 to 5:")
          case Algebra.ReadNumber =>
            S.flatMap(read) {
              s =>
                Try(s.toInt) match {
                  case Success(e) => S.pure(e)
                  case Failure(_) => E.raiseError(Error.InvalidNumber(s))
                }
            }

          case Algebra.Evaluate(target: Int, guessed: Int) =>
            //            S.pure(target == guessed)
            val interpreter = EvaluationSuite.productionInterpreter[F](S)
            EvaluationSuite.program(target, guessed)
              .foldMap(interpreter)(S)

          case Algebra.RevealResult(name: String, target: Int, r: Boolean) =>
            prompt(if (r) s"You guessed right, $name!" else s"You guessed wrong, $name! The number was: $target")

          case Algebra.PromptContinue(name: String) => prompt("Do you want to continue, " + name + "?")
          case Algebra.ReadContinue =>
            S.flatMap(read) {
              case "y" => S.pure(true)
              case "n" => S.pure(false)
              case s @ _ => E.raiseError(Error.InvalidChoice(s))
            }
          case Algebra.Finish => S.pure(())
        }
    }

  //

  object EvaluationSuite {

    sealed trait EvaluationAlgebra[A]

    object EvaluationAlgebra {
      final case class GetTarget(b: Int) extends EvaluationAlgebra[Int]
      final case class GetGuess(b: Int) extends EvaluationAlgebra[Int]
      final case class CompareThem(target: Int, guess: Int) extends EvaluationAlgebra[Boolean]
    }

    def program(target: Int, guess: Int): Free[EvaluationAlgebra, Boolean] =
      for {
        t <- Free.liftF(EvaluationAlgebra.GetTarget(target))
        g <- Free.liftF(EvaluationAlgebra.GetGuess(guess))
        r <- Free.liftF(EvaluationAlgebra.CompareThem(t, g))
      } yield r

    def productionInterpreter[F[_]](implicit S: Sync[F]): EvaluationAlgebra ~> F =
      new (EvaluationAlgebra ~> F) {
        override def apply[A](fa: EvaluationAlgebra[A]): F[A] =
          fa match {
            case EvaluationAlgebra.GetTarget(b: Int) => S.pure(b)
            case EvaluationAlgebra.GetGuess(b: Int) => S.pure(b)
            case EvaluationAlgebra.CompareThem(target: Int, guess: Int) => S.pure(target == guess)
          }
      }
  }

  /////

  private def pureMain =
    program.foldMap(productionInterpreter[EitherT[IO, Error, ?]])
      .value

  def main(args: Array[String]): Unit = {
    println(pureMain.unsafeRunSync())
  }
}
