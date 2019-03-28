package combinators

import cats.Functor
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.option._
import support.TestSupport

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
      def unshow(s: String): A
    }

    object Unshow {
      def apply[A](implicit F: Unshow[A]): Unshow[A] = F
    }

    implicit val functorUnshow: Functor[Unshow] =
      new Functor[Unshow] {
        override def map[A, B](fa: Unshow[A])(f: A => B): Unshow[B] =
          new Unshow[B] {
            override def unshow(s: String): B =
              f(fa.unshow(s))
          }
      }

    implicit val unshowStringImpl: Unshow[String] =
      new Unshow[String] {
        override def unshow(s: String): String =
          s
      }

    val unshowInt: Unshow[Int] = Unshow[String].map(_.toInt)

    unshowInt.unshow("1234")
      .assertIs(1234)

    ////

    implicit def functorFunction1[A]: Functor[A => ?] =
      new Functor[A => ?] {
        override def map[B, C](fa: A => B)(f: B => C): A => C =
          f.compose(fa)
      }

    val fab: String => Int = _.toInt
    val fac: String => Double = fab.map(i => 1.1 * i.toDouble)
    fac("1234")
      .assertIs(1234.0 * 1.1)

    ////

    val list = List(1, 2, 3)
    list
      .as(1)
      .assertIs(List(1, 1, 1))

    list
      .fmap(i => s"v:$i")
      .tupleLeft("X")
      .tupleRight("Y")
      .assertIs(List((("X", "v:1"), "Y"), (("X", "v:2"), "Y"), (("X", "v:3"), "Y")))

    list
      .fmap(i => s"v:$i")
      .fproduct(_.toUpperCase)
      .assertIs(List(("v:1", "V:1"), ("v:2", "V:2"), ("v:3", "V:3")))

    list
      .fmap(i => s"v:$i")
      .void
      .assertIs(List((), (), ()))

    ////

    implicit def functorListOption: Functor[Lambda [X => List[Option[X]]]] =
      Functor[List].compose[Option]

    val lo: List[Option[Int]] = List(1.some, None, 3.some)
    functorListOption.map(lo)((i: Int) => i + 1)
      .assertIs(List(2.some, None, 4.some))

    ()
  }
}
