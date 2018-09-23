package exp

import scala.util.Try

object DegoesSolution {

  object stdlib {
    trait Program[F[_]] {
      def finish[A](a: A): F[A]
      def chain[A, B](fa: F[A], afb: A => F[B]): F[B]
      def map[A, B](fa: F[A], ab: A => B): F[B]
    }

    object Program {
      def apply[F[_]](implicit F: Program[F]): Program[F] = F
    }

    implicit class ProgramSyntax[F[_], A](fa: F[A]) {
      def map[B](ab: A => B)(implicit F: Program[F]): F[B] = F.map(fa, ab)
      def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
    }
    def finish[F[_], A](a: A)(implicit F: Program[F]): F[A] = F.finish(a)

    final case class SimpleIO[A](unsafeRun: () => A) {
      self =>

      final def map[B](f: A => B): SimpleIO[B] = SimpleIO(() => f(self.unsafeRun()))
      final def flatMap[B](f: A => SimpleIO[B]): SimpleIO[B] =
        SimpleIO(() => f(self.unsafeRun()).unsafeRun())
    }

    object SimpleIO {
      def point[A](a: => A): SimpleIO[A] = SimpleIO(() => a)

      implicit val ProgramIO = new Program[SimpleIO] {
        def finish[A](a: A): SimpleIO[A] = SimpleIO.point(a)
        def chain[A, B](fa: SimpleIO[A], afb: A => SimpleIO[B]): SimpleIO[B] = fa.flatMap(afb)
        def map[A, B](fa: SimpleIO[A], ab: A => B): SimpleIO[B] = fa.map(ab)
      }
    }

    sealed trait ConsoleOut {
      def en: String
    }

    object ConsoleOut {
      case class YouGuessedRight(name: String) extends ConsoleOut {
        def en = "You guessed right, " + name + "!"
      }
      case class YouGuessedWrong(name: String, num: Int) extends ConsoleOut {
        def en = "You guessed wrong, " + name + "! The number was: " + num
      }
      case class DoYouWantToContinue(name: String) extends ConsoleOut {
        def en = "Do you want to continue, " + name + "?"
      }
      case class PleaseGuess(name: String) extends ConsoleOut {
        def en = "Dear " + name + ", please guess a number from 1 to 5:"
      }
      case class ThatIsNotValid(name: String) extends ConsoleOut {
        def en = "That is not a valid selection, " + name + "!"
      }
      case object WhatIsYourName extends ConsoleOut {
        def en = "What is your name?"
      }
      case class WelcomeToGame(name: String) extends ConsoleOut {
        def en = "Hello, " + name + ", welcome to the game!"
      }
    }

    trait Console[F[_]] {
      def putStrLn(line: ConsoleOut): F[Unit]
      def getStrLn: F[String]
    }

    object Console {
      def apply[F[_]](implicit F: Console[F]): Console[F] = F

      implicit val ConsoleIO = new Console[SimpleIO] {
        def putStrLn(line: ConsoleOut): SimpleIO[Unit] = SimpleIO(() => println(line.en))
        def getStrLn: SimpleIO[String] = SimpleIO(() => readLine())
      }
    }

    def putStrLn[F[_] : Console](line: ConsoleOut): F[Unit] = Console[F].putStrLn(line)
    def getStrLn[F[_] : Console]: F[String] = Console[F].getStrLn

    trait Random[F[_]] {
      def nextInt(upper: Int): F[Int]
    }

    object Random {
      def apply[F[_]](implicit F: Random[F]): Random[F] = F

      implicit val RandomIO = new Random[SimpleIO] {
        def nextInt(upper: Int): SimpleIO[Int] = SimpleIO(() => scala.util.Random.nextInt(upper))
      }
    }
    def nextInt[F[_] : Random](upper: Int): F[Int] = Random[F].nextInt(upper)
  }

  import stdlib._

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def checkAnswer[F[_] : Console](name: String, num: Int, guess: Int): F[Unit] =
    if (num == guess) putStrLn(ConsoleOut.YouGuessedRight(name)) else putStrLn(ConsoleOut.YouGuessedWrong(name, num))

  def checkContinue[F[_] : Program : Console](name: String): F[Boolean] =
    for {
      _ <- putStrLn(ConsoleOut.DoYouWantToContinue(name))
      choice <- getStrLn.map(_.toLowerCase)
      cont <- if (choice == "y") {
        finish(true)
      } else if (choice == "n") {
        finish(false)
      } else {
        checkContinue(name)
      }
    } yield cont

  def gameLoop[F[_] : Program : Console : Random](name: String): F[Unit] =
    for {
      num <- nextInt(5).map(_ + 1)
      _ <- putStrLn(ConsoleOut.PleaseGuess(name))
      guess <- getStrLn
      _ <-
        parseInt(guess)
          .fold(putStrLn(ConsoleOut.ThatIsNotValid(name)))((guess: Int) => checkAnswer(name, num, guess))
      cont <- checkContinue(name)
      _ <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def main[F[_] : Program : Console : Random]: F[Unit] =
    for {
      _ <- putStrLn(ConsoleOut.WhatIsYourName)
      name <- getStrLn
      _ <- putStrLn(ConsoleOut.WelcomeToGame(name))
      _ <- gameLoop(name)
    } yield ()

  def mainIO: SimpleIO[Unit] = main[SimpleIO]
}
