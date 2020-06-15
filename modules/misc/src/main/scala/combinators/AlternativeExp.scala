package combinators

import cats.Bifoldable
import cats.data.{ Ior, Validated }
import cats.implicits._
import support.TestSupport

import scala.collection.immutable.List

object AlternativeExp extends TestSupport {

  def main(args: Array[String]): Unit = {

    // Fold over the inner structure to combine all of the values with MonoidK.combineK

    // def unite[G[_], A](fga: F[G[A]])(implicit FM: Monad[F], G: Foldable[G]): F[A] =
    //   FM.flatMap(fga) { ga =>
    //     G.foldLeft(ga, empty[A])((acc, a) => combineK(acc, pure(a)))
    //   }

    List(Vector(1, 2), Vector(3, 4), Vector(), Vector(5, 6)).unite.assertIs(List(1, 2, 3, 4, 5, 6))

    List(1.some, 3.some, Option.empty, 5.some).unite.assertIs(List(1, 3, 5))
    List(1.some, 3.some, Option.empty, 5.some).flatten.assertIs(List(1, 3, 5)) // same as collections flatten

    List(1.asRight, 3.asRight, "asdf".asLeft, 5.asRight).unite.assertIs(List(1, 3, 5))
    List(1.rightIor, 3.rightIor, "asdf".leftIor, 5.rightIor).unite.assertIs(List(1, 3, 5))
    List(1.rightIor, 3.rightIor, "asdf".leftIor, 5.rightIor, Ior.Both("qwer", 7)).unite.assertIs(List(1, 3, 5, 7))

    ////

    // Separate the inner foldable values into the "lefts" and "rights", using bifoldMap, which uses Monoid
    Bifoldable[Either] // catsStdBitraverseForEither
    Bifoldable[Validated] // catsDataBitraverseForValidated
    Bifoldable[Tuple2] // catsStdBitraverseForTuple2
    Bifoldable[Ior] // catsBitraverseForIor

    // def separate[G[_, _], A, B](fgab: F[G[A, B]])(implicit FM: Monad[F], G: Bifoldable[G]): (F[A], F[B]) = {
    //   val as = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(pure, _ => empty[A])(algebra[A]))
    //   val bs = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(_ => empty[B], pure)(algebra[B]))
    //   (as, bs)
    // }

    List(1.asRight, "error".asLeft, "error2".asLeft, 2.asRight).separate.assertIs((List("error", "error2"), List(1, 2)))

    List(1.rightIor, "error".leftIor, "error2".leftIor, 2.rightIor)
      .separate
      .assertIs((List("error", "error2"), List(1, 2)))
    List(1.rightIor, "error".leftIor, "error2".leftIor, 2.rightIor, Ior.Both("error3", 3))
      .separate
      .toString
      .assertIs((List("error", "error2, error3"), List(1, 2, 3)).toString)  // weird equality failure without toString TODO investigate

    ////

    def isEven(i: Int) = i % 2 == 0

    isEven(2).guard[Option].assertIs(Some(()))
    isEven(3).guard[Option].assertIs(None)
    isEven(2).guard[List].assertIs(List(()))
    isEven(3).guard[List].assertIs(List())
  }
}
