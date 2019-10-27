package exp

import cats.data.{ Const, Nested, NonEmptyList, State, Tuple2K }
import cats.implicits._
import cats.{ Applicative, Monoid, Semigroup }

object Monoids {
  // List[A]
  // A => B     if B is monoid (mempty is id, mappend = .)
  // (A, B)     if A and B are monoids
  // Future[A]  if A is monoid
  // Map[A, B]

  def main(args: Array[String]): Unit = {

    // Monoid

    // TODO basic monoid demo

    // Monoids compose:
    // (a) products
    // (b) inductively

    // (a) products
    val list =
      List("Hi my name is Ingrid what is your name")
        .flatMap(_.split("""\W+"""))

    if (false) {
      val (words, chars, wordMap) = list.foldMap(word => (1, word.length, Map(word -> 1)))
      println(words)
      println(chars)
      println(wordMap)
    }

    ////

    final case class Max(i: Int)
    implicit val intMaxMonoid =
      new Monoid[Max] {
        override def empty: Max = Max(Int.MinValue)
        override def combine(x: Max, y: Max): Max = if (x.i > y.i) x else y
      }

    final case class Min(i: Int)
    implicit val intMinMonoid =
      new Monoid[Min] {
        override def empty: Min = Min(Int.MaxValue)
        override def combine(x: Min, y: Min): Min = if (x.i < y.i) x else y
      }

    if (false) {
      val (count, max, min, countMap) =
        list.foldMap(word => (1, Max(word.length), Min(word.length), Map(word.length -> Set(word))))
      println(count)
      println(max)
      println(min)
      println(countMap)
    }

    // (b) inductively

    // eg Option[A] is monoid if A is a semigroup
    //val nelMonoid1 = Monoid[Option[NonEmptyList[String]]]

    // calculate min and max without reserving in-band values Int.MaxValue or Int.MinValue
    final case class SgMax(i: Int)
    implicit val intSgMaxSemigroup =
      new Semigroup[SgMax] {
        override def combine(x: SgMax, y: SgMax): SgMax = if (x.i > y.i) x else y
      }

    final case class SgMin(i: Int)
    implicit val intSgMinSemigroup =
      new Semigroup[SgMin] {
        override def combine(x: SgMin, y: SgMin): SgMin = if (x.i < y.i) x else y
      }

    if (true) {
      val (max, min) =
        list.foldMap((s: String) => (Option(SgMax(s.length)), Option(SgMin(s.length))))
      println(max)
      println(min)
    }

    //// Applicative <=> Monoid

    // related since Applicative is lax monoidal functor - monoidal combination inside effects
    //   pure ~ empty
    //   ap / product ~ combine

    // (a) Monoid of applicative
    // (b) promote any Monoid to Applicative

    // (a) Monoid of applicative
    implicit def monoidOfApplicative[F[_], A](
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

    // (b) Applicative of Monoid ... any time Applicative is requires and only have a Monoid

    // Const has applicative instance ignores F[A => B] in the right and semigroup-combines
    // the two left sides
    val applicativeConst = Applicative[Const[String, *]]

    val egMonoid = Monoid[(Option[Int], String)]
    val egApplicative = Applicative[Const[(Option[Int], String), *]]

    //// Composition of Applicative

    // (a) Nesting
    // (b) Product

    // (a) Nesting
    import cats.data.Nested
    val nestedApplicative = Applicative[Nested[List, Option, *]]

    // (b) Product
    import cats.data.Tuple2K
    val productApplicative = Applicative[Tuple2K[List, Option, *]]

    val someApplicative = Applicative[Tuple2K[List, Nested[Nested[List, Option, *], Option, *], *]]

  }

}

object WordCount1 {

  def countChars[A](c: Char): Const[Int, A] = Const.of(1)
  def countLines[A](c: Char): Const[Int, A] = Const.of[A](if (c == '\n') 1 else 0)

  def countWords[A](c: Char): Nested[State[Boolean, *], Const[Int, *], A] =
    Nested[State[Boolean, *], Const[Int, *], A] {
      for {
        before <- State.get[Boolean]
        after = !c.isWhitespace
        _ <- State.set[Boolean](after)
      } yield {
        Const.of[A](if (!before && after) 1 else 0)
      }
    }

  def run(input: List[Char]): (Int, Int, Int) = {
    val Tuple2K(
      Tuple2K(chars: Const[Int, Unit], lines: Const[Int, Unit]),
      words: Nested[State[Boolean, *], Const[Int, *], Unit]
    ) =
      input.traverse_ {
        c =>
          val charsAndLines: Tuple2K[Const[Int, *], Const[Int, *], Unit] =
            Tuple2K[Const[Int, *], Const[Int, *], Unit](
              countChars[Unit](c),
              countLines[Unit](c)
            )

          Tuple2K[Tuple2K[Const[Int, *], Const[Int, *], *], Nested[
            State[Boolean, *],
            Const[Int, *],
            *
          ], Unit](
            charsAndLines,
            countWords[Unit](c)
          )
      }

    (lines.getConst, words.value.runA(false).value.getConst, chars.getConst)

    // Can use `Ref` instead of `State`
  }
}

object WordCount2 {
  /*
  data CharType = IsSpace | NotSpace
    deriving Show

  data Flux =
    Flux !CharType
         {-# UNPACK #-} !Int
         !CharType
    | Unknown
    deriving Show
   */

  sealed trait CharType
  object IsSpace extends CharType
  object NotSpace extends CharType

  sealed trait Flux
  case class FluxN(ctstart: CharType, count: Int, ctend: CharType) extends Flux
  case object Unknown extends Flux // mempty... could use Option[Flux] instead

  implicit val monoidFlux =
    new Monoid[Flux] {
      override def empty: Flux =
        Unknown
      override def combine(x: Flux, y: Flux): Flux =
        (x, y) match {
          case (x, Unknown) =>
            x
          case (Unknown, y) =>
            y
          case (FluxN(l, n, NotSpace), FluxN(NotSpace, nn, r)) =>
            FluxN(l, n + nn - 1, r) // split was in middle of a word... word is double-counted
          case (FluxN(l, n, _), FluxN(_, nn, r)) =>
            FluxN(l, n + nn, r) // split wasn't in middle of word
        }
    }

  def flux(c: Char): Flux =
    if (c.isSpaceChar) FluxN(IsSpace, 0, IsSpace)
    else FluxN(NotSpace, 1, NotSpace)

  def main(args: Array[String]): Unit = {
    println("a word or two or three".toCharArray.toVector.foldMap(flux))

    val vv: Vector[Flux] =
      Vector("a wo", "rd or", " tw", "o or ", "three").map {
        (s: String) =>
          s.toCharArray.toVector.foldMap(flux)
      }

    println(vv.foldMap(identity))
  }
}
