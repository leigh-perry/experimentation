package tech

trait Stringy[A] {
  def asString(t: A): String
}

object Stringy {
  def apply[A](implicit instance: Stringy[A]): Stringy[A] = instance
}

package syntax {

  trait StringySyntax {
    implicit class StringyOps[A](a: A) {
      def asString(implicit m: Stringy[A]) = {
        m.asString(a)
      }
    }
  }
}

package std {

  trait StringyInstances {
    implicit object Stringy_Int extends Stringy[Int] {
      override def asString(t: Int): String = s"int $t"
    }

    implicit object Stringy_String extends Stringy[String] {
      override def asString(t: String): String = s"string $t"
    }

    implicit def stringy_Option[A](implicit ev: Stringy[A]): Stringy[Option[A]] = {
      (t: Option[A]) => {
        t.fold("opt(-)")(
          v => {
            val s = ev.asString(v)
            s"opt($s)"
          }
        )
      }
    }

    implicit def stringy_List[A](implicit ev: Stringy[A]): Stringy[List[A]] = {
      (t: List[A]) => {
        t.foldLeft("")(
          (b, a) => {
            val s = ev.asString(a)
            if (b.isEmpty) s else s"$b, $s"
          }
        )
      }
    }
  }
}

package all {

  import tech.std.StringyInstances
  import tech.syntax.StringySyntax

  object stringy extends StringyInstances with StringySyntax

}

object TestStringy {
  def main(args: Array[String]): Unit = {

    import all.stringy._
    import scalaz.syntax.std.option._

    val noneString = None.asInstanceOf[Option[String]]
    val noneInt = None.asInstanceOf[Option[Int]]
    val o1 = 1.some
    val os = "ssss".some

    println(1.asString)
    println("ssss".asString)
    println(noneString.asString)
    println(o1.asString)
    println(os.asString)
    println(Option(o1).asString)
    println(Option(os).asString)
    println(Option(Option(noneInt)).asString)

    println(List(1, 2, 3).asString)
    println(List(List(o1, 2.some, 3.some), List(o1, noneInt, 3.some), List(o1, 2.some, 3.some)).asString)
    println(List(List(o1, 2.some, 3.some), List(o1, noneInt, 3.some), List(o1, 2.some, 3.some)).some.asString)
  }
}
