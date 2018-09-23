package redbook

import cats.Functor
import cats.Contravariant

object functors {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit def `functor for Tree`[A]: Functor[Tree] =
    new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Branch(left, right) => Branch(map(left)(f), map(right)(f))
          case Leaf(value) => Leaf(f(value))
        }
    }

  trait Printable[A] {
    def format(value: A): String
  }

  implicit def `contravariant for Printable`[A: Printable]: Contravariant[Printable] =
    new Contravariant[Printable] {
      override def contramap[A, B](fa: Printable[A])(f: B => A): Printable[B] =
        new Printable[B] {
          override def format(b: B): String = fa.format(f(b))
        }
    }
}
