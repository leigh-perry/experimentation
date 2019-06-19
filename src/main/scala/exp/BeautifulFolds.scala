package exp

import cats.effect.IO
import cats.implicits._
import cats.{Applicative, Id, Monoid, ~>}

object RankNViaNT {

  // https://apocalisp.wordpress.com/2010/07/02/higher-rank-polymorphism-in-scala/
  // Rank N Types via natural transformation

  // bar :: (forall n. n -> n) -> (Int, Double) -> (Int, Double) ; bar f (i,d) = (f i, f d)

  // createRankN :: (forall a. a -> [a]) -> b -> String -> ([b], [String]) ; createRankN f b s = (f b, f s)
  // createRankN (\a -> [a, a]) 1 "asdf"
  // ([1,1],["asdf","asdf"])

  //def createRankN[A, B](f: A => List[A], b: B, s: String): (List[B], List[String]) =
  //  (f(b), f(s))

  // Note that a method can be polymorphic in its arguments, but a value of type Function1, Function2,
  // etc, is monomorphic. So what we do is represent a rank-2 polymorphic function with a new trait
  // that accepts a type argument in its apply method
  // We can now model a function that takes a value and puts it in a list, as a natural transformation
  // from the identity functor to the List functor:

  def createRankN[B](f: Id ~> List, b: B, s: String): (List[B], List[String]) =
    (f(b), f(s))

  def main(args: Array[String]): Unit = {
    val x: (List[Double], List[String]) =
      createRankN(
        new (Id ~> List) {
          def apply[A](a: A): List[A] =
            List(a, a)
        },
        1,
        "asdf"
      )

    println(x)

    ////

  }
}

////

// :set -XExistentialQuantification
// data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)
//
// :t Fold
// Fold :: Monoid m => (i -> m) -> (m -> o) -> Fold i o

object Naive {

  final case class Fold[I, M: Monoid, O](tally: I => M, summarise: M => O)

  def fold[I, M: Monoid, O](list: List[I])(f: Fold[I, M, O]): O = {
    val reduced: M =
      list.foldLeft(Monoid[M].empty) {
        case (a, i) =>
          Monoid[M].combine(a, f.tally(i))
      }
    f.summarise(reduced)
  }

  def main(args: Array[String]): Unit = {
    val x =
      fold(List(1, 2, 3)) {
        Fold[Int, Int, Double](identity, _.toDouble)
      }

    println(x)
  }

}

////

object AdamWarski {

  trait Fold[I, O] {
    type M
    val m: Monoid[M]
    val tally: I => M
    val summarise: M => O
  }

  object Fold {
    type Aux[I, O, R] = Fold[I, O] {type M = R}

    def apply[I, MM: Monoid, O](ft: I => MM, fs: MM => O): Aux[I, O, MM] =
      new Fold[I, O] {
        override type M = MM
        override val m: Monoid[M] = Monoid[M]
        override val tally: I => M = ft
        override val summarise: M => O = fs
      }
  }

  def fold[I, O](list: List[I])(f: Fold[I, O]): O = {
    val reduced: f.M =
      list.foldLeft(f.m.empty) {
        case (a, i) =>
          f.m.combine(a, f.tally(i))
      }
    f.summarise(reduced)
  }

  def sum[M](implicit m: Monoid[M]): Fold[M, M] =
    Fold[M, M, M](identity, identity)

  def main(args: Array[String]): Unit = {

    val ints = List(1, 2, 3)

    println(fold(ints)(Fold[Int, Int, Double](identity, _.toDouble)))
    println(fold(ints)(sum))
  }
}

////

// https://github.com/atnos-org/origami

// M must have a Monad instance
// A is the type of input elements, being fed one by one to the fold
// B is the final result
// S is the type of some internal state
// start is a method to "initialize" the fold
// end is a method to "finalize" the fold
// fold is the method called for each element A and current type S

object Origami {
  trait Fold[M[_], I, O] {
    type S

    def start: M[S]
    def fold: (S, I) => M[S]
    def end(s: S): M[O]
  }
}
