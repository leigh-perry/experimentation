package redbook

import scala.annotation.tailrec

object ch03 {
  // 3.28
  //  Write a function map, analogous to the method of the same name on List, that modifies
  //    each element in a tree with a given function.

  // 3.27
  //  Write a function depth that returns the maximum path length from the root of a tree
  //    to any leaf.
  def maxDepth[A](t: Tree[A]): Int = ???


  // 3.25
  // Write a function size that counts the number of nodes (leaves and branches) in a tree
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // 3.23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => Nil
    }

  // 3.22
  def addElements(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addElements(t1, t2))
      case _ => Nil
    }
  def main(args: Array[String]): Unit = {
    println(addElements(Cons(1, Cons(2, Cons(3, Cons(3, Cons(3, Nil))))), Cons(11, Cons(12, Cons(13, Cons(14, Nil))))))
  }

  // 3.21
  def filterViaFlatmap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a: A, acc: List[B]) => foldRight(f(a), acc)((b, accb) => Cons(b, accb)))

  def main320(args: Array[String]): Unit = {
    val as = Cons(1, Cons(2, Cons(3, Nil)))
    println(flatMap(as)(a => Cons(a + 10, Cons(a + 20, Cons(a + 30, Nil)))))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
    }
  def filterViaFoldRight[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def main319(args: Array[String]): Unit = {
    println(filter(Cons(1, Cons(2, Cons(3, Nil))))(_ != 2))
  }

  // 3.18
  // Write a function map that generalizes modifying each element in a list while maintaining
  //the structure of the list
  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  // 3.15
  //  Hard: Write a function that concatenates a list of lists into a single list. Its runtime
  //    should be linear in the total length of all lists. Try to use functions we have already defined.

  // 3.14
  // Implement append in terms of either foldLeft or foldRight.
  def appendViaFoldRight[A](as: List[A], aa: A): List[A] =
    foldRight(as, Cons(aa, Nil))(Cons(_, _))

  def appendViaFoldRightL[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def main314(args: Array[String]): Unit = {
    println(appendViaFoldRightL(Cons(1, Cons(2, Cons(3, Nil))), Cons(11, Cons(12, Cons(13, Nil)))))
  }

  // 3.13
  // Can you write foldLeft in terms of foldRight? How about the other way around?
  //def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))

  // from https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/datastructures/List.scala
  //  (not stacksafe):
  //
  // build up a chain of functions which, when called, results in the operations being performed
  // with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then
  // calling the built up function with the `z` argument. Try expanding the definitions by substituting equals for equals
  // using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear. Note these implementations are
  // more of theoretical interest - they aren't stack-safe and won't work for large lists.

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldRightViaFoldLeft_1a[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    val ff: (B => B, A) => B => B = (g: B => B, a: A) => (b: B) => g(f(a, b))
    val bToB: B => B = foldLeft(l, (b: B) => b)(ff)
    bToB(z)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  def main312(args: Array[String]): Unit = {
    val a = Cons(1, Cons(2, Cons(3, Nil)))
    println(a)
    println(reverse(a))
  }

  // 3.11
  // Write sum, product, and a function to compute the length of a list using foldLeft
  def sumFoldLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def productFoldLeft(as: List[Int]): Int = foldLeft(as, 1)(_ * _)
  def lengthFoldLeft[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => b + 1)

  // 3.8
  //  See what happens when you pass Nil and Cons themselves to foldRight, like this:
  //  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)). What do you think this
  //  says about the relationship between foldRight and the data constructors of List?
  def main308(args: Array[String]): Unit = {
    val x: List[Int] =
      foldRight(List(1, 2, 3), Nil: List[Int]) {
        (a, tail1) =>
          val v: List[Int] = Cons(a, tail1)
          println(v)
          v
      }
    println(x)
  }

  // 3.7
  //  Can product, implemented using foldRight, immediately halt the recursion and
  //  return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
  //  might work if you call foldRight with a large list. This is a deeper question that we’ll
  //  return to in chapter 5.
  def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // 3.6
  //  Not everything works out so nicely. Implement a function, init, that returns a List
  //    consisting of all but the last element of a List. So, given List(1,2,3,4), init will
  //  return List(1,2,3). Why can’t this function be implemented in constant time like
  //  tail?
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("oops")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }


  // 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) {
      l
    } else {
      l match {
        case Nil => sys.error("doh")
        case Cons(_, t) => drop(t, n - 1)
      }
    }

  // 3.3
  def setHead[A](l: List[A], a: A): List[A] =
    l match {
      case Nil => sys.error("doh")
      case Cons(_, t) => Cons(a, t)
    }

  // 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("doh")
      case Cons(_, t) => t
    }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  final case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) {
        Nil
      } else {
        Cons(as.head, apply(as.tail: _*))
      }

    def sum(ints: List[Int]): Int =
      ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
      }

    def product(ds: List[Double]): Double =
      ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
      }
  }
}
