package tech.monoidnthat

import cats.data.{Const, Nested, State, Tuple2K}
import cats.syntax.foldable._
import cats.instances.int._
import cats.instances.list._

object Monoid10WordCountApplic {

  def countChars[A](c: Char): Const[Int, A] =
    Const.of(1)

  def countLines[A](c: Char): Const[Int, A] =
    Const.of[A](if (c == '\n') 1 else 0)

  def countWords[A](c: Char): Nested[State[Boolean, *], Const[Int, *], A] =
    Nested[State[Boolean, *], Const[Int, *], A] {
      // F: State[Boolean, *] ; G: Const[Int, *]
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

  val chars: List[Char] =
    """Tonight's the night
      |I shall be talking about of flu
      |the   subject   of
      |word association football
      |""".stripMargin.toList

  def main(args: Array[String]): Unit =
    println(run(chars))
}
