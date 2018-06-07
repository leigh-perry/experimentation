package tech.v2

import cats.Applicative
import cats.implicits._

object TestTraverse {
  def main(args: Array[String]): Unit = {
    val l = List(Option(1), Option(2), None)
    // val s = l.sequence

    def foldRight[A, B](fa: List[A], b: B)(f: (A, B) => B): B = {
      def loop(as: List[A]): B = {
        println(as)
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
      foldRight[A, G[List[B]]](fa, G.pure(List.empty)) {
        (a: A, b: G[List[B]]) => G.map2(f(a), b)(_ :: _)
      }

    val s: Option[List[Int]] = sequence(l)
    println(s)
  }

}
