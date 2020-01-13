package naive

final case class Async[A](register: (Either[Throwable, A] => Async[Unit]) => Async[Unit]) {
  self =>

  final def map[B](f: A => B): Async[B] =
    Async[B] {
      (callback: Either[Throwable, B] => Async[Unit]) =>
        self.register {
          case Left(e) => callback(Left(e))
          case Right(a) => callback(Right(f(a)))
        }
    }

  final def flatMap[B](f: A => Async[B]): Async[B] =
    Async[B] {
      (callback: Either[Throwable, B] => Async[Unit]) =>
        self.register {
          case Left(e) => callback(Left(e))
          case Right(a) => f(a).register(callback)
        }
    }

  final def attempt: Async[Either[Throwable, A]] =
    new Async(
      (callback: Either[Throwable, Either[Throwable, A]] => Async[Unit]) => {
        val result: Either[Throwable, A] =
          try (Right(unsafePerformIO()))
          catch {
            case t: Throwable =>
              Left(t)
          }
        callback(Right(result))
      }
    )

  final def unsafePerformIO(): A = {
    @volatile var voa: Either[Throwable, A] = null

    val handler: Either[Throwable, A] => Async[Unit] =
      (result: Either[Throwable, A]) => {
        voa = result
        Async(())
      }

    register(handler)

    voa match {
      case Left(ex) => throw ex
      case Right(a) => a
    }
  }
}

object Async {
  final def apply[A](a: => A): Async[A] =
    Async[A] {
      (callback: Either[Throwable, A] => Async[Unit]) =>
        callback(Right(a))
    }

  final def fail[A](e: Throwable): Async[A] =
    Async[A] {
      (callback: Either[Throwable, A] => Async[Unit]) =>
        callback(Left(e))
    }
}

object IO_2_App {
  def main(args: Array[String]): Unit = {
    val programSuccess =
      for {
        a <- Async("a")
        b <- Async("b")
      } yield a + b

    println(programSuccess.attempt.unsafePerformIO())

    val programFailure =
      for {
        a <- Async("a")
        b <- Async.fail(new RuntimeException("oops"))
      } yield a + b

    println(programFailure.attempt.unsafePerformIO())
  }
}
