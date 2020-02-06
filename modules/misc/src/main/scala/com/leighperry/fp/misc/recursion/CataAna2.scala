package recursion

// change to curried etc
object CataAna2 {
  def foldRight[E, B](z: B)(op: (E, B) => B): List[E] => B =
    new (List[E] => B) {
      kernel =>

      override def apply(list: List[E]): B =
        list match {
          case Nil => z
          case ::(head, tl) => op(head, kernel(tl))
        }
    }

  def unfold[E, A](f: A => Option[(E, A)]): A => List[E] =
    new (A => List[E]) {
      kernel =>

      override def apply(a: A): List[E] =
        f(a) match {
          case Some((e, a)) => e :: kernel(a)
          case None => Nil
        }
    }

  val productOp: (Int, Int) => Int =
    (x, y) => x * y

  val rangeOp: Int => Option[(Int, Int)] =
    x => if (x > 0) Some((x, x - 1)) else None

  def main(args: Array[String]): Unit = {
    println(foldRight(1)(productOp)(1 :: 10 :: 20 :: Nil))
    println(unfold(rangeOp)(10))
  }
}
