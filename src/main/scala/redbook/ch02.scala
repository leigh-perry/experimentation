package redbook

import scala.annotation.tailrec

object ch02 {
  def main(args: Array[String]): Unit = {
    println((0 to 16).map(fib))
  }

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(is: Boolean, arr: Array[A]): Boolean =
      arr.length match {
        case 0 | 1 => is
        case _ => go(is && ordered(arr(0), arr(1)), arr.slice(1, arr.length))
      }

    go(true, as)
  }

  // 2.1
  def fib(n: Int): Int = {
    @tailrec
    def loop(nn: Int, prevTotal: Int, currentTotal: Int): Int =
      if (nn == 0) prevTotal else loop(nn - 1, currentTotal, prevTotal + currentTotal)

    loop(n, 0, 1)
  }


  //  // 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987
  //  def fib(n: Int): Int = {
  //    n match {
  //      case 0 => 0
  //      case 1 => 1
  //      case x @ _ => fib(n - 2) + fib(n - 1)
  //    }
  //  }

}
