package exp

import cats.data.EitherT
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.{Applicative, Id, Monad}
import exp.DegoesFp.Error.InvalidNumber
import org.scalacheck.Prop.forAll

import scala.util.Try

// For original imperative version see App0 in https://gist.github.com/jdegoes/1b43f43e2d1e845201de853815ab3cb9

object DegoesFp {

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

  private def pureMain(args: Array[String]): IO[Either[Error, Boolean]] =
    program(new ProductionInterpreter[IO])
      .value

  def main(args: Array[String]): Unit = println(pureMain(args).unsafeRunSync())
}

////

object DegoesFpTest {

  import DegoesFp._

  def main(args: Array[String]): Unit = {
    testErrorFreeTrue()
    testErrorFreeFalse()

    testErrorAwareTrue()
    testErrorAwareFalse()
    testErrorAwareFail()
  }

  private def testErrorAwareTrue(): Unit =
    forAll {
      t: Int =>
        val target = t % 5
        val guess = target
        val alg = errorAwareAlgebra(target, guess)
        program(alg).value.unsafeRunSync() == true.asRight[Error]
    }.check()

  private def testErrorAwareFalse(): Unit =
    forAll {
      t: Int =>
        val target = t % 5
        val guess = (t + 1) % 5
        val alg = errorAwareAlgebra(target, guess)
        program(alg).value.unsafeRunSync() == false.asRight[Error]
    }.check()

  private def testErrorAwareFail(): Unit =
    forAll {
      t: Int =>
        val target = t % 5
        val guess = target
        val alg = errorAwareAlgebraFail(target, guess)
        program(alg).value.unsafeRunSync() == InvalidNumber("oops").asLeft[Int]
    }.check()

  private def testErrorFreeTrue(): Unit =
    forAll {
      t: Int =>
        val target = t % 5
        val guess = target
        val alg = errorFreeAlgebra(target, guess)
        program(alg)
    }.check()

  private def testErrorFreeFalse(): Unit =
    forAll {
      t: Int =>
        val target = t % 5
        val guess = (t + 1) % 5
        val alg = errorFreeAlgebra(target, guess)
        !program(alg)
    }.check()

  type TestResult[T] = EitherT[IO, Error, T]

  private def errorAwareAlgebra(target: Int, guess: Int): Algebra[TestResult] =
    new Algebra[TestResult] {
      override def promptName: TestResult[Unit] = EitherT.rightT(())
      override def readName: TestResult[String] = EitherT.rightT("")
      override def greet(name: String): TestResult[Unit] = EitherT.rightT(())
      override def nextNumber: TestResult[Int] = EitherT.rightT(target)
      override def promptNumber(name: String): TestResult[Unit] = EitherT.rightT(())
      override def readNumber: TestResult[Int] = EitherT.rightT(guess)
      override def evaluate(target: Int, guessed: Int): TestResult[Boolean] = EitherT.rightT(target == guessed)
      override def revealResult(name: String, guessed: Int, r: Boolean): TestResult[Unit] = EitherT.rightT(())
      override def promptContinue(name: String): TestResult[Unit] = EitherT.rightT(())
      override def readContinue: TestResult[Boolean] = EitherT.rightT(false)
    }

  private def errorAwareAlgebraFail(target: Int, guess: Int): Algebra[EitherT[IO, Error, ?]] =
    new Algebra[TestResult] {
      override def promptName: TestResult[Unit] = EitherT.rightT(())
      override def readName: TestResult[String] = EitherT.rightT("")
      override def greet(name: String): TestResult[Unit] = EitherT.rightT(())
      override def nextNumber: TestResult[Int] = EitherT.rightT(target)
      override def promptNumber(name: String): TestResult[Unit] = EitherT.rightT(())
      override def readNumber: TestResult[Int] =
        EitherT.fromOption(Try("oops".toInt).toOption, Error.InvalidNumber("oops"))

      override def evaluate(target: Int, guessed: Int): TestResult[Boolean] = EitherT.rightT(target == guessed)
      override def revealResult(name: String, guessed: Int, r: Boolean): TestResult[Unit] = EitherT.rightT(())
      override def promptContinue(name: String): TestResult[Unit] = EitherT.rightT(())
      override def readContinue: TestResult[Boolean] = EitherT.rightT(false)
    }

  private def errorFreeAlgebra(target: Int, guess: Int): Algebra[Id] =
    new Algebra[Id] {
      override def promptName: Id[Unit] = ()
      override def readName: Id[String] = ""
      override def greet(name: String): Id[Unit] = ()
      override def nextNumber: Id[Int] = target
      override def promptNumber(name: String): Id[Unit] = ()
      override def readNumber: Id[Int] = guess
      override def evaluate(target: Int, guessed: Int): Id[Boolean] = target == guessed
      override def revealResult(name: String, guessed: Int, r: Boolean): Id[Unit] = ()
      override def promptContinue(name: String): Id[Unit] = ()
      override def readContinue: Id[Boolean] = false
    }
}
