package techtalk.applicatives

import cats.Applicative
import cats.implicits._

object TestTraverse {
  def main(args: Array[String]): Unit = {
    val l = List(Option(1), Option(2), None, Option(4), Option(5))
    if (true) {
      val s = l.sequence
      //println(s)
    }

    if (true) {
      // actual impl is trampolined
      def foldRight[A, B](fa: List[A], b: B)(f: (A, B) => B): B = {
        def loop(as: List[A]): B = {
          as match {
            case Nil => b
            case h :: t => f(h, loop(t))
          }
        }

        loop(fa)
      }

      def sequence[G[_] : Applicative, A](fga: List[G[A]]): G[List[A]] =
        traverse(fga)(ga => ga)

      def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      // probably using foldRight since insertion at list head is O(1)
        foldRight[A, G[List[B]]](fa, G.pure(List.empty)) {
          (a: A, gfb: G[List[B]]) => {
            println(s"a: $a, b: $gfb")
            val gb: G[B] = f(a)
            G.map2(gb, gfb)(
              (b1, bs) => {
                println(s"(b1: $b1) :: (bs: $bs)")
                b1 :: bs
              }
            )
          }
        }

      val s: Option[List[Int]] = sequence(l)
      println(s)
    }
  }

}
