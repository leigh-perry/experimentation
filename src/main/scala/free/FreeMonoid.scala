package free

import cats.effect.IO
import cats.{ ~>, Monoid }
import cats.syntax.apply._

//sealed trait FreeMonoid[F[_], A] {
//  import FreeMonoid._
//
////  def foldMap[G[_]: Monoid](nt: F ~> G): G[A] =
////    this match {
////      case Empty() => ???
////      case Combine(fa, fb) => ???
////      case Suspend(fa) => nt(fa)
////    }
//}
//
//object FreeMonoid {
//  def liftM[F[_], A](fa: F[A]): FreeMonoid[F, A] = Suspend(fa)
//
//  final case class Empty[F[_], A]() extends FreeMonoid[F, A]
//  final case class Combine[F[_], A](fa: FreeMonoid[F, A], fb: FreeMonoid[F, A])
//    extends FreeMonoid[F, A]
//  final case class Suspend[F[_], A](fa: F[A]) extends FreeMonoid[F, A]
//
//  implicit def monoidForFreeMonoid[F[_]]: Monoid[FreeMonoid[F, *]] =
//    new Monoid[FreeMonoid[F, *]] {
//      override def empty: FreeMonoid[F, *] =
//        Empty()
//      override def combine(x: FreeMonoid[F, *], y: FreeMonoid[F, *]): FreeMonoid[F, *] =
//        Combine(x, y)
//    }
//}
//
////// sample app
//
////sealed trait MonoidOps[A]
////
////object MonoidOps {
////  final case class Read1(file: String) extends MonoidOps[Int]
////  final case class Read2(file: String) extends MonoidOps[Int]
////}
////
////object TestFreeMonoid {
////  val interpreter: MonoidOps ~> IO =
////    new (MonoidOps ~> IO) {
////      override def apply[A](fa: MonoidOps[A]): IO[A] =
////        fa match {
////          case MonoidOps.Read1(file) =>
////            IO {
////              println(fa)
////              1234
////            }
////          case MonoidOps.Read2(file) =>
////            IO {
////              println(fa)
////              2345
////            }
////        }
////    }
////
////  def main(args: Array[String]): Unit = {
////    val program: FreeMonoid[MonoidOps, Int] =
////      (FreeMonoid.liftM(MonoidOps.Read1("filename1")), FreeMonoid.liftM(MonoidOps.Read2("filename2")))
////        .mapN(_ + _)
////
////    val r: IO[Int] = program.foldMap(interpreter)
////    println(r.unsafeRunSync())
////  }
////}
