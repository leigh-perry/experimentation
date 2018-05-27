package tech

object Induction {

  trait Named[E] {
    val name: String
  }

  implicit val `Named for Int` = new Named[Int] {
    override val name: String = "Int"
  }
  implicit val `Named for Char` = new Named[Char] {
    override val name: String = "Char"
  }
  implicit val `Named for String` = new Named[String] {
    override val name: String = "String"
  }

  //  implicit val `Named for EOL` = new Named[EOL] {
  //    override val name: String = "."
  //  }

  implicit def tupleInduction[A, B](implicit eva: Named[A], evb: Named[B]): Named[(A, B)] = {
    new Named[(A, B)] {
      val name = {
        val a = eva.name
        val b = evb.name
        //println(s"evaluating $a $b")
        s"tuple($a, $b)"
      }
    }
  }

  final case class Something(e0: Int, e1: Char, e2: String, e3: Int) extends Ordered[Something] {
    override def compare(that: Something): Int = ???
  }

  val tupled: (Int, Char, String, Int) = Something.unapply(Something(1, 'c', "asdf", 2)).get // !!

  def main(args: Array[String]) = {
    println(implicitly[Named[Int]].name)
    println(implicitly[Named[(String, Int)]].name)
    println(implicitly[Named[(Int, (Char, (String, Int)))]].name)

    println("-----")
    //println(implicitly[Named[(Int, Char, String, Int)]].name)
    println(tupled)
  }

}
