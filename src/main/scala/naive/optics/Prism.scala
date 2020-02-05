package naive.optics

trait Prism[S, A] extends Serializable {
  self =>

  //def getOrModify(s: S): Either[S, A]
  def reverseGet(a: A): S
  def getOption(s: S): Option[A]

  def composePrism[B](other: Prism[A, B]): Prism[S, B] =
    ???
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
    final case class Blue(someDouble: Double) extends Colour
    object Blue {
      val prism: Prism[Colour, Double] =
        Prism.prism(
          a => Blue(a),
          (_: Colour) match {
            case Blue(a) => Some(a)
            case _ => None
          }
        )
    }
  }

  def main(args: Array[String]): Unit = {
    println(Colour.Red.prism.getOption(Colour.Red(123)))
    println(Colour.Red.prism.getOption(Colour.Green("asdf")))
    println(Colour.Green.prism.getOption(Colour.Green("asdf")))
    println(Colour.Blue.prism.getOption(Colour.Green("asdf")))

    println(Colour.Red.prism.reverseGet(234))
    println(Colour.Green.prism.reverseGet("blah"))
    println(Colour.Blue.prism.reverseGet(234.5))
  }
}
