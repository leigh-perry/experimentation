package exp

import cats.Functor
import cats.free.Free

trait Yoneda[F[_], A] {
  def apply[B](f: A => B): F[B]
}

object Yoneda {
  def toYoneda[F[_] : Functor, A](fa: F[A]): Yoneda[F, A] =
    new Yoneda[F, A] {
      override def apply[B](f: A => B): F[B] =
        Functor[F].map(fa)(f)
    }

  def fromYoneda[F[_], A](y: Yoneda[F, A]): F[A] =
    y(identity)
}

// Coyoneda[F, A] is isomorphtic to F[A]
sealed abstract class Coyoneda[F[_], A] {
  type I
  val fi: F[I]
  val k: I => A
}

object XXXX {
  type FreeC[S[_], A] = Free[Coyoneda[S, ?], A]
}