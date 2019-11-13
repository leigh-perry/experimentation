package tech.monoidnthat

import cats.Monoid
import cats.implicits._

object Monoid11WordCountFlux {
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
