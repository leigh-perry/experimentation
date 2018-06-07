package tech.v2

object ApplicativeViaProduct {

  import ApplicativeViaAp.Functor

  ////////

  trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] =
      map(product(fa, ff)) {
        case ((a, f)) => f(a)
      }

    def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
      map(product(fa, fb)) {
        case (a, b) => f(a, b)
      }


    def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] = {
      map(product(fa, product(fb, fc))) {
        case (a, (b, c)) => f(a, b, c)
      }
    }
  }

  object Applicative {
    implicit val optionApplicative: Applicative[Option] =
      new Applicative[Option] {
        def pure[A](a: A): Option[A] = Some(a)
        override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
          (fa, fb) match {
            case (Some(a), Some(b)) => Some((a, b))
            case (_, _) => None
          }
        override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
          fa.fold(None: Option[B])(a => Option(f(a)))
      }

    implicit val listApplicative: Applicative[List] =
      new Applicative[List] {
        def pure[A](a: A): List[A] = List(a)
        override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = ???
        override def map[A, B](fa: List[A])(f: A => B): List[B] = ???
      }
  }

  def main(args: Array[String]): Unit = {
    val tcP = ApplicativeViaProduct.Applicative.optionApplicative
    val rp = tcP.map3[Int, Int, Int, Int](Some(1), Some(2), Some(3))(_ + _ + _)
    println(rp)
  }
}
