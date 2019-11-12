package tech.monoidnthat

import cats.data.{Const, Nested, NonEmptyList, State, Tuple2K}
import cats.effect.{IO, Timer}
import cats.implicits._
import cats.{Applicative, Monoid, Show}

object Monoid7Applicative {

  def main(args: Array[String]): Unit = {

    // TODO
    // val x = implicitly[MonoidK[Function1[Int, *]]]

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

    val egApplicativeFromMonoid = Applicative[Const[(Option[Int], String), *]]

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

object SortedListMonoid {

  final case class SortedList[A: Ordering](list: List[A])

  object SortedList {
    implicit def showInstance[A: Ordering]: Show[SortedList[A]] =
      new Show[SortedList[A]] {
        override def show(t: SortedList[A]): String =
          t.toString
      }
  }

  implicit def sortedListMonoid[A: Ordering]: Monoid[SortedList[A]] =
    new Monoid[SortedList[A]] {
      override def empty: SortedList[A] =
        SortedList(List.empty[A])

      override def combine(x: SortedList[A], y: SortedList[A]): SortedList[A] =
        SortedList(mergeSort(x.list, y.list))
    }

  def mergeSort[A: Ordering](x: List[A], y: List[A]): List[A] =
    (x, y) match {
      case (Nil, y) => y
      case (x, Nil) => x
      case (xh :: xt, yh :: yt) if Ordering[A].compare(xh, yh) < 1 =>
        xh :: mergeSort(xt, y)
      case (xh :: xt, yh :: yt) =>
        yh :: mergeSort(x, yt)
    }

  def main(args: Array[String]): Unit = {
    import fs2.Stream

    import scala.concurrent.ExecutionContext
    import scala.concurrent.duration.DurationInt
    implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

    val count = 10
    Stream
      .fixedRate[IO](500.milliseconds)
      .zip(Stream.range[IO](1, count))
      .map(count - _._2)
      .map(i => SortedList(List(i)))
      .scanMonoid
      .showLinesStdOut
      .compile
      .drain
      .unsafeRunSync()
  }
}
