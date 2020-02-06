package free

import cats.Monoid

sealed trait FreeMonoid[+A] {

//  def foldMap[G[_]: Monoid](nt: F ~> G): G[A] =
//    this match {
//      case Empty() => ???
//      case Combine(fa, fb) => ???
//      case Suspend(fa) => nt(fa)
//    }
}

object FreeMonoid {
  def liftM[A](a: A): FreeMonoid[A] = Suspend(a)

  final case object Empty extends FreeMonoid[Nothing]
  final case class Combine[A](fa: FreeMonoid[A], fb: FreeMonoid[A]) extends FreeMonoid[A]
  final case class Suspend[A](a: A) extends FreeMonoid[A]

  implicit def monoidForFreeMonoid[A]: Monoid[FreeMonoid[A]] =
    new Monoid[FreeMonoid[A]] {
      override def empty: FreeMonoid[A] =
        Empty
      override def combine(x: FreeMonoid[A], y: FreeMonoid[A]): FreeMonoid[A] =
        Combine(x, y)
    }
}

//// sample app

//sealed trait MonoidOps[A]
//
//object MonoidOps {
//  final case class Read1(file: String) extends MonoidOps[Int]
//  final case class Read2(file: String) extends MonoidOps[Int]
//}
//
//object TestFreeMonoid {
//  val interpreter: MonoidOps ~> IO =
//    new (MonoidOps ~> IO) {
//      override def apply[A](fa: MonoidOps[A]): IO[A] =
//        fa match {
//          case MonoidOps.Read1(file) =>
//            IO {
//              println(fa)
//              1234
//            }
//          case MonoidOps.Read2(file) =>
//            IO {
//              println(fa)
//              2345
//            }
//        }
//    }
//
//  def main(args: Array[String]): Unit = {
//    val program: FreeMonoid[MonoidOps, Int] =
//      (FreeMonoid.liftM(MonoidOps.Read1("filename1")), FreeMonoid.liftM(MonoidOps.Read2("filename2")))
//        .mapN(_ + _)
//
//    val r: IO[Int] = program.foldMap(interpreter)
//    println(r.unsafeRunSync())
//  }
//}
