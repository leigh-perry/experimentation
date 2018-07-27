package exp

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad, MonadError}

// For original imperative version see App0 in https://gist.github.com/jdegoes/1b43f43e2d1e845201de853815ab3cb9

object DegoesFpGist {

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
      _ <- if (more) loop(alg, name) else Applicative[F].pure(false)
    } yield r

  ////

  sealed trait Error
  object Error {
    final case class InvalidNumber(s: String) extends Error
    final case class InvalidChoice(s: String) extends Error
  }

  class ProductionInterpreter[F[_]](implicit S: Sync[F], E: MonadError[F, Error]) extends Algebra[F] {

    import scala.io.StdIn.readLine
    import scala.util.{Failure, Success, Try}

    private def prompt(s: String): F[Unit] = S.delay(print(s"$s "))
    private def read: F[String] = S.delay(readLine())

    override def promptName: F[Unit] = prompt(s"Enter name:")
    override def readName: F[String] = read
    override def greet(name: String): F[Unit] = prompt(s"Hello $name")
    override def nextNumber: F[Int] = S.delay(scala.util.Random.nextInt(5) + 1)
    override def promptNumber(name: String): F[Unit] = prompt(s"Dear $name, please guess a number from 1 to 5:")
    override def readNumber: F[Int] =
      S.flatMap(read) {
        s =>
          Try(s.toInt) match {
            case Success(e) => S.pure(e)
            case Failure(_) => E.raiseError(Error.InvalidNumber(s))
          }
      }

    override def evaluate(target: Int, guessed: Int): F[Boolean] = S.pure(target == guessed)
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

  import cats.data.EitherT
  import cats.effect.IO

  private def pureMain(args: Array[String]) =
    program(new ProductionInterpreter[EitherT[IO, Error, ?]])
      .value

  def main(args: Array[String]): Unit = println(pureMain(args).unsafeRunSync())
}
