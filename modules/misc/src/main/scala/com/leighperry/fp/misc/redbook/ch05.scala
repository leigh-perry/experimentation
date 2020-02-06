package redbook

import cats.Eval

object ch05 {

  import Stream._

  def main(args: Array[String]): Unit = {
    //    val s = streamOf((0 to 9).toList)
    //    assert(s.toList == s.toListRec)
    //    println(s.take(5).toList)
    //    println(s.drop(5).toList)
    //    println(s.takeWhile(_ < 7).toList)
    //    println(s.takeWhileFoldRight(_ < 7).toList)
    //    println(s.headOption)
    //    println("headOption " + cons(0, cons(1, cons(2, empty))).headOption)
    //    println("headOption " + cons(0, cons(1, empty)).headOption)
    //    println("headOption " + cons(0, empty).headOption)
    //    println("headOption " + empty.headOption)
    //    println(s.map(_ * 2).toList)
    //    println(s.append(s).toList)
    //    println(s.flatMap(a => cons(a, cons(a, empty))).toList)
    //    println(s.filter(_ != 5).toList)
    //
    //    println(constant(1).take(4).toList)
    //    println(from(12).take(4).toList)
    //    println(fibs.take(10).toList)

    println(onesF.take(4).toList)
    println(fromF(12).take(4).toList)
    println(fibs.take(10).toList)
    println(fibsF.take(10).toList)
  }

  val ones: Stream[Int] = cons(1, ones)

  // 5.12
  // fibs, from, constant, and ones in terms of unfold.
  val onesF: Stream[Int] = unfold[Int, Unit](())(s => Some((1, ())))
  def fromF(n: Int): Stream[Int] = unfold[Int, Int](n)(s => Some((s, s + 1)))
  def fibsF: Stream[Int] = unfold[Int, (Int, Int)]((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  // 5.11
  // Write a more general stream-building function called unfold. It takes an initial state,
  // and a function for producing both the next state and the next value in the generated
  // stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((aa, ss)) => cons(aa, unfold(ss)(f))
    }
  }

  // 5.10
  // function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on
  def fibs: Stream[Int] = {
    def go(p: Int, i: Int): Stream[Int] = cons(p, go(i, p + i))

    go(0, 1)
  }

  // 5.09
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.08
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def streamOf[A](l: List[A]): Stream[A] = l.foldRight(empty[A])((a, acc) => cons(a, acc))

  sealed trait Stream[+A] {
    // 5.13 Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and
    // zipAll. The zipAll function should continue the traversal as long as either stream
    // has more elements—it uses Option to indicate whether each stream has been
    // exhausted.
    // def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])]
    //foldRight(empty[B])((a, acc) => cons(f(a), acc))
    // Cheated...
    def mapF[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }

    // 5.07 Implement map, filter, append, and flatMap using foldRight. The append method
    // should be non-strict in its argument
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B]) {
        (a, acc) => cons(f(a), acc)
      }
    def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, acc) => cons(h, acc))
    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) {
        (a, acc) => if (p(a)) cons(a, acc) else acc
      }
    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B]) {
        (a, acc) => f(a).append(acc)
      }

    // 5.06 Implement headOption using foldRight :tick:
    def headOption: Option[A] =
      foldRight(None: Option[A]) {
        (a, _) => Option(a)
      }

    // 5.05 Use foldRight to implement takeWhile.
    def takeWhileFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) {
        (a, acc) =>
          if (p(a)) cons(a, acc) else empty
      }

    // 5.04
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    // NB page 71 - lazy evaluation of foldRight used to enable short-circuiting
    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        // If f doesn’t evaluate its second argument (ie ... || `b`), the recursion never occurs
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    // 5.03
    def takeWhile(p: A => Boolean): Stream[A] =
      this match {
        case Empty => Empty
        case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
      }

    // 5.02
    def drop(n: Int): Stream[A] =
      if (n == 0) {
        this
      } else {
        this match {
          case Empty => Empty
          case Cons(h, t) => t().drop(n - 1)
        }
      }

    // 5.02
    def take(n: Int): Stream[A] =
      if (n == 0) {
        Empty
      } else {
        this match {
          case Empty => Empty
          case Cons(h, t) => Cons(h, () => t().take(n - 1))
        }
      }

    // 5.01
    //@tailrec
    def toListRec: List[A] =
      this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toListRec
      }

    // 5.01 stacksafe using trampolining
    def toList: List[A] = {
      import cats.Eval._
      def go(s: Stream[A]): Eval[List[A]] =
        s match {
          case Empty => now(Nil)
          case Cons(h, t) =>
            for {
              hh <- now(h())
              tt <- go(t())
            } yield hh :: tt
        }

      go(this).value
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
