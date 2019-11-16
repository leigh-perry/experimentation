package free

import cats.{Monad, ~>}

sealed trait FreeAp[F[_], A] {
  import FreeAp._

  def foldMap[G[_]: Monad](nt: F ~> G): G[A] =
    this match {
      case Pure(a) => Monad[G].pure(a)
    }
}

object FreeAp {
  def pure[F[_], A](a: A): FreeAp[F, A] = Pure(a)
  def liftM[F[_], A](fa: F[A]): FreeAp[F, A] = Suspend(fa)

  final case class Pure[F[_], A](a: A) extends FreeAp[F, A]
  final case class Suspend[F[_], A](fa: F[A]) extends FreeAp[F, A]
}

//// sample app

//trait Imperative {
//  def read(file: String): String
//  def write(file: String, contents: String): Unit
//}

//sealed trait Ops[A]
//
//object Ops {
//  final case class Read(file: String) extends Ops[Int]
//  final case class Write(file: String, contents: Int) extends Ops[Unit]
//}
//
//object TestFreeAp {
//  val interpreter: Ops ~> IO =
//    new (Ops ~> IO) {
//      override def apply[A](fa: Ops[A]): IO[A] =
//        fa match {
//          case Ops.Read(file) =>
//            IO {
//              println("reading")
//              1234
//            }
//          case Ops.Write(file, contents) =>
//            IO {
//              println(s"writing $contents")
//            }
//        }
//    }
//
//  def main(args: Array[String]): Unit = {
//    val program: FreeAp[Ops, Unit] =
//      for {
//        i <- FreeAp.liftM(Ops.Read("filename"))
//        _ <- FreeAp.liftM(Ops.Write("filename", i))
//      } yield ()
//
//    val r: IO[Unit] = program.foldMap(interpreter)
//    println(r.unsafeRunSync())
//  }
//}
