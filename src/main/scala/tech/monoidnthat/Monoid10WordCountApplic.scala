package tech.monoidnthat

import cats.data.{ Const, Nested, State, Tuple2K }
import cats.implicits._

object Monoid10WordCountApplic {

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
