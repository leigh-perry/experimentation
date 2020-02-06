package recursion

object CataAna1 {
  def foldRight[E, B](init: List[E])(z: B)(op: (E, B) => B): B =
    init match {
      case Nil => z
      case ::(head, tl) => op(head, foldRight(tl)(z)(op))
    }

  def unfold[E, A](init: A)(f: A => Option[(E, A)]): List[E] =
    f(init) match {
      case Some((e, a)) => e :: unfold(a)(f)
      case None => Nil
    }

  val productOp: (Int, Int) => Int =
    (x, y) => x * y

  val expandedString: Int => Option[(String, Int)] =
    x => if (x < 5) Some((s"val:$x", x + 1)) else None

  val rangeOp: Int => Option[(Int, Int)] =
    x => if (x > 0) Some((x, x - 1)) else None

  def main(args: Array[String]): Unit = {
    println(foldRight(1 :: 10 :: 20 :: Nil)(1)(productOp))
    println(unfold(0)(expandedString))
    println(unfold(10)(rangeOp))
  }
}
