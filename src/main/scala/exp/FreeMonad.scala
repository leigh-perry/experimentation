package exp

import cats.effect.IO
import cats.{ ~>, Monad }

sealed trait FreeMonad[F[_], A] {
  import FreeMonad._

  def flatMap[B](f: A => FreeMonad[F, B]): FreeMonad[F, B] = FlatMap(this, f)
  def map[B](f: A => B): FreeMonad[F, B] = flatMap(a => pure(f(a)))

  def foldMap[G[_]: Monad](nt: F ~> G): G[A] =
    this match {
      case Pure(a) => Monad[G].pure(a)
      case Suspend(fa) => nt(fa)
      case FlatMap(target, f) =>
        // f: A => FreeMonad[F, B]
        val ga: G[A] = target.asInstanceOf[FreeMonad[F, A]].foldMap(nt)
        Monad[G].flatMap(ga)(a => f(a).foldMap(nt))
    }
}

object FreeMonad {
  def pure[F[_], A](a: A): FreeMonad[F, A] = Pure(a)
  def liftM[F[_], A](fa: F[A]): FreeMonad[F, A] = Suspend(fa)

  final case class Pure[F[_], A](a: A) extends FreeMonad[F, A]
  final case class Suspend[F[_], A](fa: F[A]) extends FreeMonad[F, A]
  final case class FlatMap[F[_], A, B](target: FreeMonad[F, A], f: A => FreeMonad[F, B])
    extends FreeMonad[F, B]
}

//// sample app

//trait Imperative {
//  def read(file: String): String
//  def write(file: String, contents: String): Unit
//}

sealed trait Ops[A]

object Ops {
  final case class Read(file: String) extends Ops[Int]
  final case class Write(file: String, contents: Int) extends Ops[Unit]
}

object TestFreeMonad {
  val interpreter: Ops ~> IO =
    new (Ops ~> IO) {
      override def apply[A](fa: Ops[A]): IO[A] =
        fa match {
          case Ops.Read(file) =>
            IO {
              println("reading")
              1234
            }
          case Ops.Write(file, contents) =>
            IO {
              println(s"writing $contents")
            }
        }
    }

  def main(args: Array[String]): Unit = {
    val program: FreeMonad[Ops, Unit] =
      for {
        i <- FreeMonad.liftM(Ops.Read("filename"))
        _ <- FreeMonad.liftM(Ops.Write("filename", i))
      } yield ()

    val r: IO[Unit] = program.foldMap(interpreter)
    println(r.unsafeRunSync())
  }
}
