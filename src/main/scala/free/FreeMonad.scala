package free

import cats.effect.IO
import cats.{ ~>, Monad }
import cats.syntax.functor._
import cats.syntax.flatMap._

sealed trait FreeMonad[F[_], A] {
  import FreeMonad._

  def foldMap[G[_]: Monad](nt: F ~> G): G[A] =
    this match {
      case Pure(a) => Monad[G].pure(a)

      case FlatMap(target, f) =>  // f: A => FreeMonad[F, B]
        val ga: G[A] = target.asInstanceOf[FreeMonad[F, A]].foldMap(nt)
        Monad[G].flatMap(ga)(a => f(a).foldMap(nt))

      case Suspend(fa) => nt(fa)
    }
}

object FreeMonad {
  def liftM[F[_], A](fa: F[A]): FreeMonad[F, A] = Suspend(fa)

  final case class Pure[F[_], A](a: A) extends FreeMonad[F, A]
  final case class FlatMap[F[_], A, B](target: FreeMonad[F, A], f: A => FreeMonad[F, B])
    extends FreeMonad[F, B]
  final case class Suspend[F[_], A](fa: F[A]) extends FreeMonad[F, A]

  implicit def monadForFreeMonad[F[_]]: Monad[FreeMonad[F, *]] =
    new Monad[FreeMonad[F, *]] {
      override def pure[A](x: A): FreeMonad[F, A] =
        FreeMonad.Pure(x)
      override def flatMap[A, B](fa: FreeMonad[F, A])(f: A => FreeMonad[F, B]): FreeMonad[F, B] =
        FreeMonad.FlatMap(fa, f)
      override def tailRecM[A, B](a: A)(f: A => FreeMonad[F, Either[A, B]]): FreeMonad[F, B] =
        flatMap(f(a)) {
          case Left(a0) => tailRecM(a0)(f)
          case Right(b) => pure(b)
        }
    }
}

//// sample app

//trait Imperative {
//  def read(file: String): String
//  def write(file: String, contents: String): Unit
//}

sealed trait FmOps[A]

object FmOps {
  final case class Read(file: String) extends FmOps[Int]
  final case class Write(file: String, contents: Int) extends FmOps[Unit]
}

object TestFreeMonad {
  val interpreter: FmOps ~> IO =
    new (FmOps ~> IO) {
      override def apply[A](fa: FmOps[A]): IO[A] =
        fa match {
          case FmOps.Read(file) =>
            IO {
              println("reading")
              1234
            }
          case FmOps.Write(file, contents) =>
            IO {
              println(s"writing $contents")
            }
        }
    }

  def main(args: Array[String]): Unit = {
    val program: FreeMonad[FmOps, Unit] =
      for {
        i <- FreeMonad.liftM(FmOps.Read("filename"))
        _ <- FreeMonad.liftM(FmOps.Write("filename", i))
      } yield ()

    val r: IO[Unit] = program.foldMap(interpreter)
    println(r.unsafeRunSync())
  }
}
