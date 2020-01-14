package naive

final case class Async[A](register: (Either[Throwable, A] => Unit) => Unit) {
  self =>

  def map[B](f: A => B): Async[B] =
    Async[B] {
      (callback: Either[Throwable, B] => Unit) =>
        self.register {
          case Left(e) => callback(Left(e))
          case Right(a) => callback(Right(f(a)))
        }
    }

  def flatMap[B](f: A => Async[B]): Async[B] =
    Async[B] {
      (callback: Either[Throwable, B] => Unit) =>
        self.register {
          case Left(e) => callback(Left(e))
          case Right(a) => f(a).register(callback)
        }
    }

  def attempt: Async[Either[Throwable, A]] =
    new Async(
      (callback: Either[Throwable, Either[Throwable, A]] => Unit) => {
        val result: Either[Throwable, A] =
          try (Right(unsafeRunAsync()))
          catch {
            case t: Throwable =>
              Left(t)
          }
        callback(Right(result))
      }
    )

  def unsafeRunAsync(): A = {
    @volatile var voa: Either[Throwable, A] = null

    register {
      (result: Either[Throwable, A]) =>
        // TODO wait on asynchronous completion with CountDownLatch
        voa = result
        ()
    }

    voa match {
      case Left(ex) => throw ex
      case Right(a) => a
    }
  }
}

object Async {
  def apply[A](a: => A): Async[A] =
    Async[A] {
      (callback: Either[Throwable, A] => Unit) =>
        callback(Right(a))
    }

  def fail[A](e: Throwable): Async[A] =
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

    println(programSuccess.attempt.unsafeRunAsync())

    val programFailure =
      for {
        a <- Async("a")
        b <- Async.fail(new RuntimeException("oops"))
      } yield a + b

    println(programFailure.attempt.unsafeRunAsync())
  }
}
