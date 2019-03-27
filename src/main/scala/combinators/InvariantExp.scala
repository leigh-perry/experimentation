package combinators

import cats.Invariant
import cats.syntax.invariant._
import support.TestSupport

object InvariantExp
  extends TestSupport {
  def main(args: Array[String]): Unit = {

    trait Codec[A] {
      def decode(s: String): A
      def encode(a: A): String
    }

    object Codec {
      def apply[A](implicit F: Codec[A]): Codec[A] = F
    }

    implicit val invariantCodec: Invariant[Codec] =
      new Invariant[Codec] {
        override def imap[A, B](fa: Codec[A])(f: A => B)(g: B => A): Codec[B] =
          new Codec[B] {
            override def decode(s: String): B =
              f(fa.decode(s))
            override def encode(b: B): String =
              fa.encode(g(b))
          }
      }

    implicit val codecStringImpl: Codec[String] =
      new Codec[String] {
        override def decode(s: String): String =
          s
        override def encode(a: String): String =
          a
      }

    val codecInt: Codec[Int] = Codec[String].imap(_.toInt)(_.toString)

    codecInt.decode("1234")
      .shouldBe(1234)
    codecInt.encode(1234)
      .shouldBe("1234")

    ()
  }
}
