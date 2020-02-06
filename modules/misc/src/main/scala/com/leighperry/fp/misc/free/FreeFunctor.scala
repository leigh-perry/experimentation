package free

import cats.effect.IO
import cats.{ ~>, Functor }
import cats.syntax.functor._

sealed trait FreeFunctor[F[_], A] {
  import FreeFunctor._

  def foldMap[G[_]: Functor](nt: F ~> G): G[A] =
    this match {

      case Map(fa, f) => // f: A => B
        Functor[G].map(fa.foldMap(nt))(f)

      case Suspend(fa) => nt(fa)
    }
}

// TODO reduce and implement map fusion

object FreeFunctor {
  def liftM[F[_], A](fa: F[A]): FreeFunctor[F, A] = Suspend(fa)

  final case class Map[F[_], A, B](fa: FreeFunctor[F, A], f: A => B) extends FreeFunctor[F, B]
  final case class Suspend[F[_], A](fa: F[A]) extends FreeFunctor[F, A]

  implicit def applicativeForFreeFunctor[F[_]]: Functor[FreeFunctor[F, *]] =
    new Functor[FreeFunctor[F, *]] {
      override def map[A, B](fa: FreeFunctor[F, A])(f: A => B): FreeFunctor[F, B] =
        Map(fa, f)
    }
}

//// sample app

sealed trait FunctorOps[A]

object FunctorOps {
  final case class SomeStep(seed: String) extends FunctorOps[Int]
}

object TestFreeFunctor {
  val interpreter: FunctorOps ~> IO =
    new (FunctorOps ~> IO) {
      override def apply[A](fa: FunctorOps[A]): IO[A] =
        fa match {
          case FunctorOps.SomeStep(value) =>
            IO(value.length)
        }
    }

  def main(args: Array[String]): Unit = {
    import FunctorOps._

    val program: FreeFunctor[FunctorOps, String] =
      FreeFunctor
        .liftM(SomeStep("1234567890123"))
        .map(_ / 2.0)
        .map(math.sqrt(_))
        .map(_.toString)

    val r: IO[String] = program.foldMap(interpreter)
    println(r.unsafeRunSync())
  }
}
