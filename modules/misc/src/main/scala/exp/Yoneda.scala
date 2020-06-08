package exp

import cats.Functor
import cats.instances.list._

// See https://medium.com/@olxc/yoneda-and-coyoneda-trick-f5a0321aeba4

trait Yoneda[F[_], A] {
  self =>

  def transform[B](f: A => B): F[B]

  def map[B](f: A => B): Yoneda[F, B] =
    new Yoneda[F, B] {
      override def transform[C](g: B => C): F[C] =
        self.transform(g compose f)
    }

  def run: F[A] =
    transform(identity)
}

object Yoneda {
  def toYoneda[F[_]: Functor, A](fa: F[A]): Yoneda[F, A] =
    new Yoneda[F, A] {
      override def transform[B](f: A => B): F[B] =
        Functor[F].map(fa)(f)
    }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    val y = Yoneda.toYoneda(list)
    val y2 = y.map(_ + 1).map(_ * 2).map(_ + 10)
    val list2 = y2.run
    println(list2)
  }

}

sealed trait Coyoneda[F[_], A] {
  type UnderlyingType
  val underlyingValue: F[UnderlyingType]
  val transformation: UnderlyingType => A

  def run(f: Functor[F]): F[A] =
    f.map(underlyingValue)(transformation)

  def map[B](f: A => B): Coyoneda[F, B] =
    Coyoneda.toCoyoneda(underlyingValue)(f compose transformation)
}

object Coyoneda {
  def toCoyoneda[F[_], A, B](fa: F[A])(f: A => B): Coyoneda[F, B] =
    new Coyoneda[F, B] {
      // save original type
      override type UnderlyingType = A
      override val transformation: A => B =
        f
      override val underlyingValue: F[A] =
        fa
    }
}

//// Coyoneda[F, A] is isomorphic to F[A]
//sealed abstract class Coyoneda[F[_], A] {
//  type I
//  val fi: F[I]
//  val k: I => A
//}

//  type FreeC[S[_], A] = Free[Coyoneda[S, *], A]
