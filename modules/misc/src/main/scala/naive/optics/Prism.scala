package naive.optics

trait Prism[S, A] {
  self =>

  def reverseGet(a: A): S
  def getOption(s: S): Option[A]

  def modify(f: A => A): S => S =
    s =>
      getOption(s)
        .fold(s)(a => reverseGet(f(a)))

  // S may map to an A, A may map to a B
  def composePrism[B](other: Prism[A, B]): Prism[S, B] =
    new Prism[S, B] {
      override def reverseGet(b: B): S =
        self.reverseGet(other.reverseGet(b))

      override def getOption(s: S): Option[B] =
        self
          .getOption(s)
          .fold[Option[B]](None) {
            a =>
              other.getOption(a)
          }
    }
}

object Prism {
  def prism[S, A](revGetter: A => S, getter: S => Option[A]): Prism[S, A] =
    new Prism[S, A] {
      override def reverseGet(a: A): S =
        revGetter(a)

      override def getOption(s: S): Option[A] =
        getter(s)
    }
}

////

object PrismTest {
  sealed trait Colour
  object Colour {
    final case class Red(someInt: Int) extends Colour
    object Red {
      val prism: Prism[Colour, Int] =
        Prism.prism(
          a => Red(a),
          (_: Colour) match {
            case Red(a) => Some(a)
            case _ => None
          }
        )
    }

    final case class Green(someString: String) extends Colour
    object Green {
      val prism: Prism[Colour, String] =
        Prism.prism(
          a => Green(a),
          (_: Colour) match {
            case Green(a) => Some(a)
            case _ => None
          }
        )
    }

    final case class Blue(luminance: Option[Double]) extends Colour
    object Blue {
      val prism: Prism[Colour, Option[Double]] =
        Prism.prism(
          a => Blue(a),
          (_: Colour) match {
            case Blue(a) => Some(a)
            case _ => None
          }
        )
    }
  }

  def prismOptional[A]: Prism[Option[A], A] =
    Prism.prism(Option(_), identity)

  def main(args: Array[String]): Unit = {
    println(Colour.Red.prism.getOption(Colour.Red(123)))
    println(Colour.Red.prism.getOption(Colour.Green("asdf")))
    println(Colour.Green.prism.getOption(Colour.Green("asdf")))
    println(Colour.Blue.prism.getOption(Colour.Green("asdf")))
    println("--------")

    println(Colour.Red.prism.reverseGet(234))
    println(Colour.Green.prism.reverseGet("blah"))
    println(Colour.Blue.prism.reverseGet(Some(234.5)))
    println(Colour.Blue.prism.reverseGet(None))
    println("--------")

    val pBlueLuminance: Prism[Colour, Double] =
      Colour.Blue.prism.composePrism(prismOptional[Double])

    println(pBlueLuminance.getOption(Colour.Blue(Some(456.7))))
    println(pBlueLuminance.getOption(Colour.Blue(None)))
    println(pBlueLuminance.getOption(Colour.Red(123)))
    println("--------")

    println(pBlueLuminance.modify(_ * 1000.0)(Colour.Blue(Some(456.7))))
    println(pBlueLuminance.modify(_ * 1000.0)(Colour.Blue(None)))
    println(pBlueLuminance.modify(_ * 1000.0)(Colour.Red(123)))
  }
}
