package tech.v2

import cats.implicits._


object ApplicativeViaCats {

  def main(args: Array[String]): Unit = {
    // Cats
    val r: Option[Int] = (Option(3), Option(5)).mapN(_ + _)
    println(r)

    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
      fa.flatMap(a => fb.map(b => (a, b)))

    def map2[A0, A1, Z](f0: Option[A0], f1: Option[A1])(f: (A0, A1) => Z): Option[Z] =
      product(f0, f1).map { case (a0, a1) => f(a0, a1) }

    map2((Option(3), Option(5))._1, (Option(3), Option(5))._2)(_ + _)

    // Tuple2SemigroupalOps
    //   def mapN[Z](f: (A0, A1) => Z)(implicit functor: Functor[F], semigroupal: Semigroupal[F]): F[Z] = Semigroupal.map2(t2._1, t2._2)(f)
    //
    // SemigroupalArityFunctions
    //   /** @group MapArity */
    //   def map2[F[_], A0, A1, Z](f0:F[A0], f1:F[A1])(f: (A0, A1) => Z)(implicit semigroupal: Semigroupal[F], functor: Functor[F]): F[Z] =
    //     functor.map(semigroupal.product(f0, f1)) { case (a0, a1) => f(a0, a1) }
    //
    // FlapMap
    //   override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    //     flatMap(fa)(a => map(fb)(b => (a, b)))
    //
    // OptionInstances
    //       def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
    //         fa.flatMap(f)
    ()
  }
}
