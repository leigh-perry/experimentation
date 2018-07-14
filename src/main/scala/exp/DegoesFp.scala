package exp

import cats.Monad
import cats.data.EitherT
import cats.effect.{IO, Sync}

import scala.util.Try

// For original imperative version see App0 in https://gist.github.com/jdegoes/1b43f43e2d1e845201de853815ab3cb9

object DegoesFp {

  trait Algebra[F[_], E] {
    def promptName: EitherT[F, E, Unit]
    def readName: EitherT[F, E, String]
    def greet(name: String): EitherT[F, E, Unit]
    def nextNumber: EitherT[F, E, Int]
    def promptNumber(name: String): EitherT[F, E, Unit]
    def readNumber: EitherT[F, E, Int]
    def evaluate(target: Int, guessed: Int): EitherT[F, E, Boolean]
    def revealResult(name: String, guessed: Int, r: Boolean): EitherT[F, E, Unit]
    def promptContinue(name: String): EitherT[F, E, Unit]
    def readContinue: EitherT[F, E, Boolean]
  }

  def program[F[_] : Monad, E](alg: Algebra[F, E]): EitherT[F, E, Unit] =
    for {
      _ <- alg.promptName
      name <- alg.readName
      _ <- alg.greet(name)
      _ <- loop(alg, name)
    } yield ()

  def loop[F[_] : Monad, E](alg: Algebra[F, E], name: String): EitherT[F, E, Unit] =
    for {
      target <- alg.nextNumber
      _ <- alg.promptNumber(name)
      guessed <- alg.readNumber
      r <- alg.evaluate(target, guessed)
      _ <- alg.revealResult(name, target, r)
      _ <- alg.promptContinue(name)
      more <- alg.readContinue
      _ <- if (more) loop(alg, name) else EitherT.rightT[F, E](())
    } yield ()

  ////

  sealed trait Error
  object Error {
    final case class InvalidNumber(s: String) extends Error
    final case class InvalidChoice(s: String) extends Error
  }

  class ProductionInterpreter[F[_] : Sync] extends Algebra[F, Error] {

    import scala.io.StdIn.readLine

    private def prompt(s: String): EitherT[F, Error, Unit] = EitherT.liftF(Sync[F].delay(print(s"$s ")))
    private def read: EitherT[F, Error, String] = EitherT.liftF(Sync[F].delay(readLine()))

    override def promptName: EitherT[F, Error, Unit] = prompt(s"Enter name:")
    override def readName: EitherT[F, Error, String] = read
    override def greet(name: String): EitherT[F, Error, Unit] = prompt(s"Hello $name")
    override def nextNumber: EitherT[F, Error, Int] =
      EitherT.liftF(Sync[F].delay(scala.util.Random.nextInt(5) + 1))

    override def promptNumber(name: String): EitherT[F, Error, Unit] =
      prompt(s"Dear $name, please guess a number from 1 to 5:")

    override def readNumber: EitherT[F, Error, Int] =
      read.flatMap(s => EitherT.fromOption(Try(s.toInt).toOption, Error.InvalidNumber(s)))

    override def evaluate(target: Int, guessed: Int): EitherT[F, Error, Boolean] = EitherT.rightT(target == guessed)
    override def revealResult(name: String, target: Int, r: Boolean): EitherT[F, Error, Unit] =
      prompt(if (r) s"You guessed right, $name!" else s"You guessed wrong, $name! The number was: $target")

    override def promptContinue(name: String): EitherT[F, Error, Unit] = prompt("Do you want to continue, " + name + "?")
    override def readContinue: EitherT[F, Error, Boolean] =
      read.flatMap {
        case "y" => EitherT.rightT(true)
        case "n" => EitherT.rightT(false)
        case s @ _ => EitherT.leftT(Error.InvalidChoice(s))
      }
  }

  private def pureMain(args: Array[String]): IO[Either[Error, Unit]] =
    program(new ProductionInterpreter[IO])
      .value

  def main(args: Array[String]): Unit = println(pureMain(args).unsafeRunSync())
}
