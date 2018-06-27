package tech.v1

object Functors {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Contravariant[F[_]] {
    def contramap[A, B](r: F[A])(f: B => A): F[B]
  }

}
