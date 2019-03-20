package exp

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

object RefExp {

  def memoise[A](io: IO[A]): IO[IO[A]] =
    for {
      ref <- Ref[IO].of(io)
      _ <-
        ref.update {
          ioa =>
            ioa.flatTap(a => ref.set(IO.pure(a)))
        }
    } yield ref.get.flatten

  val program = IO {
    println("Hey!");
    42
  }

  val exec = for {
    io <- memoise(program)
    x <- io
    y <- io
  } yield (x, y)

  def main(args: Array[String]): Unit = {
    // Prints Hey!, then (42,42)
    println(exec.unsafeRunSync())
  }

}
