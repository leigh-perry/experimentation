package combinators

import cats.Apply
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.option._
import support.TestSupport

object ApplyExp
  extends TestSupport {
  def main(args: Array[String]): Unit = {
    //    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
    //    def productR[B](fb : F[B]) : F[B]
    //    def productL[B](fb : F[B]) : F[C]
    //    final def <*>[A, B](ff: F[A => B])(fa: F[A]): F[B]
    //    def *>[B](fb : F[B]) : F[B]
    //    def <*[B](fb : F[B]) : F[C]
    //    ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z]
    //    def map2[B, Z](fb : F[B])(f : scala.Function2[C, B, Z]) : F[Z]
    //    def map2Eval[B, Z](fb : cats.Eval[F[B]])(f : scala.Function2[C, B, Z]) : cats.Eval[F[Z]]

    val f: Int => Int => Int = a => b => a + b

    (1.some *> 2.some)
      .shouldBe(Some(2))

    (1.some.productR(2.some))
      .shouldBe(Some(2))

    (1.some <* 2.some)
      .shouldBe(Some(1))

    (1.some.productL(2.some))
      .shouldBe(Some(1))

    (None *> 2.some)
      .shouldBe(None)

    (1.some <* None)
      .shouldBe(None)

    (Apply[Option].map2(1.some, 2.some)(_ + _))
      .shouldBe(Some(3))

    ((1.some, 2.some).mapN(_ + _))
      .shouldBe(Some(3))

    ((1.some, 2.some, 3.some).mapN(_ + _ + _))
      .shouldBe(Some(6))

    (11.some.fmap(f).ap(12.some))
      .shouldBe(Some(23))

    (11.some.fmap(f) <*> 12.some)
      .shouldBe(Some(23))

    (11.some.fmap(f) <*> None)
      .shouldBe(None)

    ()
  }
}
