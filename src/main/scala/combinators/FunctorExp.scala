package combinators

import cats.{Functor, Show}
import cats.instances.list._
import cats.syntax.functor._
import testsupport.TestSupport

object FunctorExp
  extends TestSupport {
  def main(args: Array[String]): Unit = {
    //      def map[A, B](fa: F[A])(f: A => B): F[B]
    //      override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = map(fa)(f)
    //
    //      // derived methods
    //      final def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)
    //      def widen[A, B >: A](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
    //      def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
    //      def void[A](fa: F[A]): F[Unit] = as(fa, ())
    //      def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))
    //      def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
    //      def tupleLeft[A, B](fa: F[A], b: B): F[(B, A)] = map(fa)(a => (b, a))
    //      def tupleRight[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)(a => (a, b))
    //
    //      def compose[G[_]: Functor]: Functor[λ[α => F[G[α]]]] =
    //      override def composeContravariant[G[_]: Contravariant]: Contravariant[λ[α => F[G[α]]]] =

    trait Unshow[A] {
      def read(s: String): A
    }

    object Unshow {
      def apply[A](implicit F: Unshow[A]): Unshow[A] = F
    }

    implicit def readIntImpl: Unshow[String] =
      new Unshow[String] {
        override def read(s: String): String =
          s
      }

    implicit def functorUnshow =
      new Functor[Unshow] {
        override def map[A, B](fa: Unshow[A])(f: A => B): Unshow[B] =
          new Unshow[B] {
            override def read(s: String): B =
              f(fa.read(s))
          }
      }

    val readInt: Unshow[Int] = Unshow[String].map(_.toInt)

    readInt.read("1234")
      .shouldBe(1234)

    ////

    val list = List(1, 2, 3)
    list
      .as(1)
      .shouldBe(List(1, 1, 1))

    list
      .fmap(i => s"v:$i")
      .tupleLeft("X")
      .tupleRight("Y")
      .shouldBe(List((("X", "v:1"), "Y"), (("X", "v:2"), "Y"), (("X", "v:3"), "Y")))

    list
      .fmap(i => s"v:$i")
      .fproduct(_.toUpperCase)
      .shouldBe(List(("v:1", "V:1"), ("v:2", "V:2"), ("v:3", "V:3")))

    list
      .fmap(i => s"v:$i")
      .void
      .shouldBe(List((), (), ()))

    ()
  }
}
