package naive.optics

import cats.data.Const
import cats.instances.int._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.{ Applicative, Id, Monoid, Traverse }

// A Traversal is the generalisation of an Optional to several targets –
// allows to focus from a type S into 0-n values of type A
trait Traversal[S, A] {
  self =>

  /** all traversal methods are written in terms of modifyF */
  def modifyF[F[_]: Applicative](f: A => F[A])(s: S): F[S]

  def foldMap[M: Monoid](f: A => M)(s: S): M =
    modifyF[Const[M, *]](a => Const[M, A](f(a)))(s).getConst

  def fold(s: S)(implicit ev: Monoid[A]): A =
    foldMap(identity)(s)

  def getAll(s: S): List[A] =
    foldMap(List(_))(s)

  def find(p: A => Boolean): S => Option[A] = {
    implicit val monoidOptionA: Monoid[Option[A]] = Traversal.firstOption[A]
    foldMap(a => Option(a).filter(p))
  }

  def headOption(s: S): Option[A] = {
    implicit val monoidOptionA: Monoid[Option[A]] = Traversal.firstOption[A]
    foldMap(a => Option[A](a))(s)
  }

  def lastOption(s: S): Option[A] = {
    implicit val monoidOptionA: Monoid[Option[A]] = Traversal.lastOption[A]
    foldMap(a => Option[A](a))(s)
  }

  def exist(p: A => Boolean): S => Boolean = {
    implicit val monoidBoolean: Monoid[Boolean] = Traversal.anyBoolean
    foldMap(p)
  }

  def all(p: A => Boolean): S => Boolean = {
    implicit val monoidBoolean: Monoid[Boolean] = Traversal.allBoolean
    foldMap(p)
  }

  def length(s: S): Int =
    foldMap(_ => 1)(s)

  def isEmpty(s: S): Boolean = {
    //length(s) == 0
    implicit val monoidBoolean: Monoid[Boolean] = Traversal.allBoolean
    foldMap(_ => false)(s)
  }

  def nonEmpty(s: S): Boolean = {
    //!isEmpty(s)
    implicit val monoidBoolean: Monoid[Boolean] = Traversal.anyBoolean
    foldMap(_ => true)(s)
  }

  def modify(f: A => A): S => S =
    modifyF[Id](f)

  def set(a: A): S => S =
    modify(_ => a)

  def composeTraversal[B](other: Traversal[A, B]): Traversal[S, B] =
    new Traversal[S, B] {
      override def modifyF[F[_]: Applicative](f: B => F[B])(s: S): F[S] =
        // self T[S, A]  -- (A => F[A]) => S => F[S]
        // other T[A, B] -- (B => F[B]) => A => F[A]
        // f: B => F[B]
        // s: S
        self.modifyF(a => other.modifyF(f)(a))(s)
    }

}

object Traversal {
  def fromTraverse[S[_]: Traverse, A]: Traversal[S[A], A] =
    new Traversal[S[A], A] {
      override def modifyF[F[_]: Applicative](f: A => F[A])(s: S[A]): F[S[A]] =
        Traverse[S].traverse(s)(f)
    }

  ////

  def firstOption[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def empty: Option[A] =
        None

      override def combine(x: Option[A], y: Option[A]): Option[A] =
        x orElse y
    }

  def lastOption[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def empty: Option[A] =
        None

      override def combine(x: Option[A], y: Option[A]): Option[A] =
        y orElse x
    }

  val anyBoolean: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean =
        false

      override def combine(x: Boolean, y: Boolean): Boolean =
        x || y
    }

  val allBoolean: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean =
        true

      override def combine(x: Boolean, y: Boolean): Boolean =
        x && y
    }
}

object TraversalTest {
  def main(args: Array[String]): Unit = {
    if (true) {
      val tr: Traversal[List[Int], Int] = Traversal.fromTraverse[List, Int]

      val list = List(1, 2, 3, 4, 5)

      println(tr.modify(_ * 10)(list))
      println(tr.set(66)(list))

      println(tr.getAll(list))
      println(tr.headOption(list))
      println(tr.lastOption(list))

      println(tr.length(list))
      println(tr.isEmpty(list))
      println(tr.nonEmpty(list))

      println(tr.find(_ % 2 == 0)(list))
      println(tr.all(_ < 5)(list))
      println(tr.exist(_ < 5)(list))
    }

    val tr1: Traversal[List[List[String]], List[String]] = Traversal.fromTraverse[List, List[String]]
    val tr2: Traversal[List[String], String] = Traversal.fromTraverse[List, String]
    val listList = List(List("a", "b"), List("c", "d", "e"), List("f", "g", "h", "i"))

    if (true) {
      val tr: Traversal[List[List[String]], String] = tr1.composeTraversal(tr2)

      println(tr.modify(_.toUpperCase)(listList))
      println(tr.find(_ > "d")(listList))
    }

    if (true) {
      def selectively[G[_]: Traverse, A](p: A => Boolean): Traversal[G[A], A] =
        new Traversal[G[A], A] {
          def modifyF[F[_]](f: A => F[A])(s: G[A])(implicit F: Applicative[F]): F[G[A]] =
            s.traverse {
              case a if p(a) => f(a)
              case a => a.pure[F]
            }
        }

      val tr = tr1.composeTraversal(selectively(_ > "d"))
      println(tr.modify(_.toUpperCase)(listList))
    }
  }
}
