package exp

object TaglessFinal {
  def main(args: Array[String]): Unit = {
    println(s"ADT ${calcExpAdt(Add(Lit(50), Neg(Add(Lit(6), Lit(2)))))}")

    println(s"tagless final add ${expressionUsingAdd[Int]}")
    println(s"tagless final add ${expressionUsingAdd[String]}")
    println(s"tagless final add-mul ${expressionUsingAddMul[Int]}")
    println(s"tagless final add-mul ${expressionUsingAddMul[String]}")

    val o: Wrapper[String] = expressionUsingAdd[Wrapper[String]]
    println(o.description)
    val transformed: String = o.transform(P)
    println(s"tagless final adder $transformed")
  }

  sealed trait Adder[T] {
    def lit(i: Int): T
    def neg(e: T): T
    def add(l: T, r: T): T
  }

  sealed trait Multiplier[T] {
    def mul(l: T, r: T): T
  }

  def expressionUsingAdd[T](implicit adder: Adder[T]): T = {
    import adder._
    add(lit(50), neg(add(lit(6), lit(2))))
  }

  def expressionUsingAddMul[T](
    implicit adder: Adder[T],
    multiplier: Multiplier[T]
  ): T = {
    import adder._
    import multiplier._
    mul(lit(2), add(lit(1), add(lit(10), lit(10))))
  }

  // define interpreter as instance
  implicit val adderImplInt: Adder[Int] =
    new Adder[Int]() {
      override def lit(i: Int) = i
      override def neg(e: Int) = -e
      override def add(l: Int, r: Int) = l + r
    }
  implicit val multiplierImplInt: Multiplier[Int] =
    new Multiplier[Int]() {
      override def mul(l: Int, r: Int) = l * r
    }
  implicit val adderImplString: Adder[String] =
    new Adder[String]() {
      override def lit(i: Int) = i.toString
      override def neg(e: String) = s"-$e"
      override def add(l: String, r: String) = s"($l + $r)"
    }
  implicit val multiplierImplString: Multiplier[String] =
    new Multiplier[String]() {
      override def mul(l: String, r: String) = s"($l * $r)"
    }

  ////////////////////

  sealed trait Ctx
  case object P extends Ctx
  case object N extends Ctx

  // newtype for Ctx => A
  final class Wrapper[T](val description: String, val transform: Ctx => T)

  def optNeg[T](adder: Adder[T]): Adder[Wrapper[T]] = {
    new Adder[Wrapper[T]] {
      override def lit(i: Int): Wrapper[T] = {
        new Wrapper[T](
          i.toString, {
            case P => adder.lit(i)
            case N => adder.neg(adder.lit(i))
          }
        )
      }

      override def neg(e: Wrapper[T]): Wrapper[T] = {
        new Wrapper[T](
          s"neg(${e.description})", {
            case P => e.transform(N)
            case N => e.transform(P)
          }
        )
      }

      override def add(l: Wrapper[T], r: Wrapper[T]): Wrapper[T] = {
        new Wrapper[T](
          s"add(${l.description}, ${r.description})",
          ctx => adder.add(l.transform(ctx), r.transform(ctx))
        )
      }
    }
  }

  implicit def adderImplOptString: Adder[Wrapper[String]] = optNeg(implicitly[Adder[String]])

  ////////////////////

  sealed trait ExpAdt
  case class Lit(i: Int) extends ExpAdt
  case class Neg(i: ExpAdt) extends ExpAdt
  case class Add(l: ExpAdt, r: ExpAdt) extends ExpAdt

  def calcExpAdt(exp: ExpAdt): Int = {
    exp match {
      case Lit(i) => i
      case Neg(e) => -calcExpAdt(e)
      case Add(l, r) => calcExpAdt(l) + calcExpAdt(r)
    }
  }
}
