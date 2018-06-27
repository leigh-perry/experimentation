package techtalk.applicatives

import scalaz.Scalaz._

object ApplicativeViaScalaz {

  def main(args: Array[String]): Unit = {
    // Scalaz
    val r: Option[Int] = (Option(3) |@| Option(5))(_ + _)
    println(r)

    def apply2[A, B, C](fa: => Option[A], fb: => Option[B])(f: (A, B) => C): Option[C] =
      ap2(fa, fb)(Option(f))

    def ap[A, B](fa: => Option[A])(f: => Option[A => B]) =
      f match {
        case Some(f) => fa match {
          case Some(x) => Some(f(x))
          case None => None
        }
        case None => None
      }

    def ap2[A, B, C](fa: => Option[A], fb: => Option[B])(f: Option[(A, B) => C]): Option[C] =
      ap(fb)(ap(fa)(f.map(_.curried)))


    final class ApplicativeBuilder[A, B](a: Option[A], b: Option[B]) {
      def apply[C](f: (A, B) => C): Option[C] = apply2(a, b)(f)
    }

    val c = new ApplicativeBuilder[Int, Int](Option(3), Option(5))
    val result = c(_ + _)
    println(result)
  }
}
