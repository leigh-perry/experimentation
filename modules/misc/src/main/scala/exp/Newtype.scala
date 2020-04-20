package exp

trait Newtype[A] {
  type Type <: A
  type Super >: A
  def apply(a: A): Type
  def toF[F[_]](fa: F[A]): F[Type]
  def fromF[F[_]](fa: F[Type]): F[A]
}

object Newtype {
  def of[A]: Newtype[A] =
    new Newtype[A] {
      override type Type = A
      override def apply(a: A): Type = a
      override def toF[F[_]](fa: F[A]): F[Type] = fa
      override def fromF[F[_]](fa: F[Type]): F[A] = fa
    }
}

object NewtypeApp {

  val Mult: Newtype[Int] = Newtype.of[Int]
  type Mult = Mult.Type

  object C1 {

    val mult1: Mult = Mult(1)
    val asInt1: Int = mult1

    val list: List[Mult] = Mult.toF(List(1, 2, 3))

    ////

    trait Contrav[-A] {
      def something(x: A): Boolean
    }

    implicit val contraInt: Contrav[Int] = ???
    implicit val contraMult: Contrav[Mult] = contraInt // Mult.toF(contraInt)

    val resultMult: Boolean = implicitly[Contrav[Mult]].something(Mult(1))
    val resultInt: Boolean = implicitly[Contrav[Int]].something(1)
  }

  ////

  object C2 {
    type MultS = Mult.Super

    val mult2: MultS = 2
    val list: List[MultS] = List(1, 2, 3) // Mult.toF(List(1, 2, 3))

    ////

    trait Cov[+A] {
      def something(x: Boolean): A
    }

    implicit val covMult: Cov[Mult] = ???
    implicit val covInt: Cov[Int] = covMult   // Mult.fromF(covMult)

    val resultMult: Mult = implicitly[Cov[Mult]].something(true)
    val resultMultS: MultS = implicitly[Cov[MultS]].something(true)
    val resultInt: Int = implicitly[Cov[Int]].something(true)
  }
}
