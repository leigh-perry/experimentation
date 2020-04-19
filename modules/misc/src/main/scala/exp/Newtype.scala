package exp

trait Newtype[A] {
  type Type <: A
  def apply(a: A): Type
  def toF[F[_]](fa: F[A]): F[Type]
  def fromF[F[_]](fa: F[Type]): F[A]
}

object Newtype {
  def of[A]: Newtype[A] =
    new Newtype[A] {
      type Type = A
      override def apply(a: A): Type = a
      override def toF[F[_]](fa: F[A]): F[Type] = fa
      override def fromF[F[_]](fa: F[Type]): F[A] = fa
    }

}

object NewtypeApp {

  val Mult: Newtype[Int] = Newtype.of[Int]
  type Mult = Mult.Type

  val list: List[Mult] = Mult.toF(List(1, 2, 3))

  ////

  trait Contrav[-A] {
    def something(x: A): Boolean
  }

  implicit val contraInt: Contrav[Int] = ???
  val contraMult1: Contrav[Mult] = Mult.toF(contraInt)
  implicit val contraMult2: Contrav[Mult] = contraInt

  val resultMult: Boolean = implicitly[Contrav[Mult]].something(Mult(1))
  val resultInt: Boolean = implicitly[Contrav[Int]].something(1)

  ////

  trait Cov[+A] {
    def something(x: Boolean): A
  }

  implicit val covMult: Cov[Mult] = ???
  val covInt: Cov[Int] = Mult.fromF(covMult)
  implicit val covInt2: Cov[Int] = covMult
}
