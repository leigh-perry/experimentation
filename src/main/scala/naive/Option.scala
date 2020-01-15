package naive

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(a) => f(a)
      case None => None
    }
}

object Option {
  def empty[A]: Option[A] =
    None
}

final case class Some[A](a: A) extends Option[A]
final case object None extends Option[Nothing]

////

object OptionApp {
  def main(args: Array[String]): Unit = {
    val r =
      for {
        a <- Some("a")
        b <- Some("b")
      } yield a + b

    println(r)
  }
}
