package demo


object ApplicativeViaAp {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  ////

  object Functor {
    implicit val listFunctor: Functor[List] =
      new Functor[List] {
        def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
      }
    implicit val optionFunctor: Functor[Option] =
      new Functor[Option] {
        def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
      }
  }

  ////////

  trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))

    def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = {
      val ffb: F[B => A => Z] = pure(b => a => f(a, b))
      val ffa: F[A => Z] = ap(fb)(ffb)
      ap(fa)(ffa)
    }

    def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] = {

      val fffa: F[A => B => C => Z] = pure(f.curried)
      val fffb: F[B => C => Z] = ap(fa)(fffa)
      val fffc: F[C => Z] = ap(fb)(fffb)
      ap(fc)(fffc)

      ap(fc)(ap(fb)(ap(fa)(pure(f.curried))))

      val ffc: F[C => B => A => Z] = pure(c => b => a => f(a, b, c))
      val ffb: F[B => A => Z] = ap(fc)(ffc)
      val ffa: F[A => Z] = ap(fb)(ffb)
      ap(fa)(ffa)

      ap(fa)(ap(fb)(map(fc)(c => b => a => f(a, b, c))))
    }

    def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => Z): F[Z] = {
      val ffd: F[D => C => B => A => Z] = pure(d => c => b => a => f(a, b, c, d))
      val ffc: F[C => B => A => Z] = ap(fd)(ffd)
      val ffb: F[B => A => Z] = ap(fc)(ffc)
      val ffa: F[A => Z] = ap(fb)(ffb)
      ap(fa)(ffa)
    }
  }

  object Applicative {
    implicit val optionApplicative: Applicative[Option] =
      new Applicative[Option] {
        override def pure[A](a: A): Option[A] = Some(a)
        override def ap[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = (fa, ff) match {
          case (None, _) => None
          case (Some(a), None) => None
          case (Some(a), Some(f)) => Some(f(a))
        }
      }
    implicit val listApplicative: Applicative[List] =
      new Applicative[List] {
        override def pure[A](a: A): List[A] = List(a)
        override def ap[A, B](fa: List[A])(ff: List[A => B]): List[B] =
          for {
            a <- fa
            f <- ff
          } yield f(a)
      }
  }
}

////////////////

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
}

object Xplore {

  def main(args: Array[String]): Unit = {
    val tcAp = ApplicativeViaAp.Applicative.optionApplicative
    val rap = tcAp.map3[Int, Int, Int, Int](Some(1), Some(2), Some(3))(_ + _ + _)
    println(rap)

    val tcP = ApplicativeViaProduct.Applicative.optionApplicative
    val rp = tcP.map3[Int, Int, Int, Int](Some(1), Some(2), Some(3))(_ + _ + _)
    println(rp)
  }

}
