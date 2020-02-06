package exp

import cats.Id

object PairingExp {
  def fapply[A, B](f: A => B)(a: A): B =
    f(a)

  def uncurry[A, B, C](f: A => B => C)(ab: (A, B)): C =
    f(ab._1)(ab._2)

  ////

  trait Pairing[F[_], G[_]] {
    // Apply: def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]
    // ... two different type constructors
    // ... returns value not container

    // liftA2 :: (a -> b -> c) -> f a -> f b -> f c

    def pair[A, B, C](fa: F[A], gb: G[B])(f: (A, B) => C): C
  }

  val pairIdId: Pairing[Id, Id] =
    new Pairing[Id, Id] {
      override def pair[A, B, C](fa: Id[A], gb: Id[B])(f: (A, B) => C): C =
        f(fa, gb)
    }

  def pairTupleApply[X] =
    new Pairing[(X, ?), X => ?] {
      override def pair[A, B, C](fa: (X, A), gb: X => B)(f: (A, B) => C): C =
        f(fa._2, gb(fa._1))
    }

  ////

  case class Coproduct[F[_], G[_], A](run: Either[F[A], G[A]])
  case class Product[F[_], G[_], A](fst: F[A], snd: G[A])

  // have implicit `Pairing` for the first of each Coproduct/Product and for second of each Coproduct/Product
  def product[F1[_], F2[_], G1[_], G2[_]](
    implicit P1: Pairing[F1, G1],
    P2: Pairing[F2, G2]
  ): Pairing[Coproduct[F1, F2, ?], Product[G1, G2, ?]] =
    new Pairing[Coproduct[F1, F2, ?], Product[G1, G2, ?]] {
      override def pair[A, B, C](ffa: Coproduct[F1, F2, A], ggb: Product[G1, G2, B])(f: (A, B) => C): C =
        ffa.run match {
          case Left(f1a) =>
            P1.pair(f1a, ggb.fst)(f)

          case Right(f2a) =>
            P2.pair(f2a, ggb.snd)(f)
        }
    }

}
