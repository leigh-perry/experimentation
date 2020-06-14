package combinators

import cats.implicits._
import cats.{ Distributive, Representable }
import support.TestSupport

object DistributiveExp extends TestSupport {

  def main(args: Array[String]): Unit = {

    val ints = List(1, 2, 3)

    // collect :: Functor f =>            (a -> f b) -> g a -> f (g b)
    // def distribute[G[_]: Functor, A, B](ga: G[A])(f: A => F[B]): F[G[B]]

    // distribute :: Functor g => g (f a) -> f (g a)
    // def cosequence[G[_]: Functor, A](ga: G[F[A]]): F[G[A]] = distribute(ga)(identity)

    // Distributive[Function[Long, *]]
    val f1: Int => Long => String = i => long => "x" * (i + long.toInt)
    val long2List: Long => List[String] = ints.distribute(f1)
    long2List(2L).assertIs(List("xxx", "xxxx", "xxxxx"))

    ////

    val representativeInstance: Representable[λ[α => (α, α)]] = Representable[λ[α => (α, α)]]

    // nested scope is needed here...weird
    {
      val f2: Int => (String, String) = (i: Int) => ("x" * i, "y" * i)

      val mapped: List[(String, String)] = ints.map(f2)
      mapped.assertIs(List(("x","y"), ("xx","yy"), ("xxx","yyy")))

      // Tuple2 Applicative uses Semigroup to combine the left values, ie:
      //    val x = s.combine(fa._1, fb._1)
      //    (x, (fa._2, fb._2))
      val traversed: (String, List[String]) = ints.traverse(f2)
      traversed.assertIs(("xxxxxx", List("y", "yy", "yyy")))

      implicit val distributiveInstance: Distributive[λ[α => (α, α)]] =
        Representable.distributive[λ[α => (α, α)]](representativeInstance)

      // Distributive[λ[α => (α, α)]]
      // def distribute[G[_]: Functor, A, B](ga: G[A])(f: A => F[B]): F[G[B]]
      val lists: (List[String], List[String]) = ints.distribute[λ[α => (α, α)], String](f2)
      lists.assertIs((List("x", "xx", "xxx"), List("y", "yy", "yyy")))
    }
  }
}
