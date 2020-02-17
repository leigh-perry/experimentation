package base

trait Applicative[F[_]] extends Functor[F] {

  def pure[A](x: A): F[A]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  //// Haskell-idiomatic <*>

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    map(product(ff, fa)) {
      case (f, a) =>
        f(a)
    }

  //// lift functions

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    map(product(fa, fb)) {
      f.tupled
    }

  def map3[A0, A1, A2, Z](f0: F[A0], f1: F[A1], f2: F[A2])(f: (A0, A1, A2) => Z): F[Z] =
    map(product(f0, product(f1, f2))) {
      case (a0, (a1, a2)) => f(a0, a1, a2)
    }

  def map4[A0, A1, A2, A3, Z](f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3])(f: (A0, A1, A2, A3) => Z): F[Z] =
    map(product(f0, product(f1, product(f2, f3)))) {
      case (a0, (a1, (a2, a3))) => f(a0, a1, a2, a3)
    }

  def map5[A0, A1, A2, A3, A4, Z](f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3], f4: F[A4])(
    f: (A0, A1, A2, A3, A4) => Z
  ): F[Z] =
    map(product(f0, product(f1, product(f2, product(f3, f4))))) {
      case (a0, (a1, (a2, (a3, a4)))) => f(a0, a1, a2, a3, a4)
    }

  // etc
}
