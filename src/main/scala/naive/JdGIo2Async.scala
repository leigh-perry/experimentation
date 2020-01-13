package naive

final case class Async[A](register: (Either[Throwable, A] => Unit) => Unit) {
  self =>

  final def map[B](f: A => B): Async[B] =
    Async[B] {
      (callback: Either[Throwable, B] => Unit) =>
        self.register {
          case Left(e) => callback(Left(e))
          case Right(a) => callback(Right(f(a)))
        }
    }

  final def flatMap[B](f: A => Async[B]): Async[B] =
    Async[B] {
      (callback: Either[Throwable, B] => Unit) =>
        self.register {
          case Left(e) => callback(Left(e))
          case Right(a) => f(a).register(callback)
        }
    }

  final def attempt: Async[Either[Throwable, A]] =
    new Async(
      (callback: Either[Throwable, Either[Throwable, A]] => Unit) => {
        val result: Either[Throwable, A] =
          try (Right(unsafePerformAsync()))
          catch {
            case t: Throwable =>
              Left(t)
          }
        callback(Right(result))
      }
    )

  final def unsafePerformAsync(): A = {
    @volatile var voa: Either[Throwable, A] = null

    val handler: Either[Throwable, A] => Unit =
      (result: Either[Throwable, A]) => {
        voa = result
        ()
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
      (callback: Either[Throwable, A] => Unit) =>
        callback(Right(a))
    }

  final def fail[A](e: Throwable): Async[A] =
    Async[A] {
      (callback: Either[Throwable, A] => Unit) =>
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

    println(programSuccess.attempt.unsafePerformAsync())

    val programFailure =
      for {
        a <- Async("a")
        b <- Async.fail(new RuntimeException("oops"))
      } yield a + b

    println(programFailure.attempt.unsafePerformAsync())
  }
}
