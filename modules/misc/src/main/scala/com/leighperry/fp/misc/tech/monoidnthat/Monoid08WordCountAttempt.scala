package tech.monoidnthat

import cats.instances.int._
import cats.instances.list._
import cats.instances.tuple._
import cats.syntax.foldable._

object Monoid08WordCountAttempt {

  val chars: List[Char] =
    """Tonight's the night
      |I shall be talking about of flu
      |the   subject   of
      |word association football
      |""".stripMargin.toList

  def main(args: Array[String]): Unit = {
    val (ls, cs) = chars.foldMap(c => (if (c == '\n') 1 else 0, 1))
    println(s"$ls $cs")
  }
}
