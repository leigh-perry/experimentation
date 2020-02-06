package tech.monoidnthat

import cats.Monoid
import cats.instances.list._
import cats.syntax.foldable._

object Monoid09WordCountCustom {

  final case class Counts(lines: Int, inWord: Boolean, words: Int, chars: Int)
  object Counts {
    implicit val monoidInstance =
      new Monoid[Counts] {
        override def empty: Counts =
          Counts(0, false, 0, 0)
        override def combine(x: Counts, y: Counts): Counts =
          Counts(
            x.lines + y.lines,
            y.inWord,
            x.words + y.words + (if (!x.inWord && y.inWord) 1 else 0),
            x.chars + y.chars
          )
      }
  }

  val chars: List[Char] =
    """Tonight's the night
      |I shall be talking about of flu
      |the   subject   of
      |word association football
      |""".stripMargin.toList

  def main(args: Array[String]): Unit = {
    val counts =
      chars.foldMap(
        c =>
          Counts(
            lines = if (c == '\n') 1 else 0,
            inWord = c != ' ' && c != '\n',
            words = 0,
            chars = 1
          )
      )
    println(counts)
  }

}
