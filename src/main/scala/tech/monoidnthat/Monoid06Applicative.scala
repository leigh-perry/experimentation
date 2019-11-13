package tech.monoidnthat

import cats.data.{ Const, Nested, NonEmptyList, State, Tuple2K }
import cats.effect.{ IO, Timer }
import cats.implicits._
import cats.{ Applicative, Monoid, Show }

object Monoid06Applicative {

  def main(args: Array[String]): Unit = {

    //// Applicative <=> Monoid

    // related since Applicative is lax monoidal functor - monoidal combination inside effects
    //   pure ~ empty
    //   ap / product ~ combine

    // (a) Monoid instance for any applicative
    // (b) promote any Monoid to Applicative

    // (a) Monoid of applicative
    implicit def monoidForApplicative[F[_], A](
      implicit F: Applicative[F],
      M: Monoid[A]
    ): Monoid[F[A]] =
      new Monoid[F[A]] {
        override def empty: F[A] =
          F.pure(M.empty)
        override def combine(x: F[A], y: F[A]): F[A] =
          F.map2(x, y)(M.combine)
      }

    val nelMonoid2 = Monoid[NonEmptyList[String]]

    // (b) promote any Monoid to Applicative ... any time Applicative is required but only have a Monoid

    // Const has applicative instance ignores F[A => B] in the right and semigroup-combines
    // the two left sides
    val applicativeConst = Applicative[Const[String, *]]

    val egApplicativeFromMonoid1 = Applicative[Const[String, *]]
    val egApplicativeFromMonoid2 = Applicative[Const[Option[Int], *]]
    val egApplicativeFromMonoid3 = Applicative[Const[(Option[Int], String), *]]
  }
}
