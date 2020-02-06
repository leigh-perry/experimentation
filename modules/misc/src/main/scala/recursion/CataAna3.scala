package recursion

// ()-----------z-------->B
// (E,B)--------op------->B
// Either[(), (E,B)]----->B
// Option[(E,B)]----f---->B
object CataAna3 {
  // `foldRight` signature is now very close to `unfold`
//def unfold   [E, B](f: B => Option[(E, B)]): B => List[E] =
  def foldRight[E, B](f: Option[(E, B)] => B): List[E] => B =
    new (List[E] => B) {
      kernel =>

      override def apply(list: List[E]): B =
        list match {
          case Nil => f(None)
          case ::(head, tl) => f(Some((head, kernel(tl))))
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

  val productOp: Option[(Int, Int)] => Int =
    _ match {
      case Some((x, y)) => x * y
      case None => 1
    }

  val rangeOp: Int => Option[(Int, Int)] =
    x => if (x > 0) Some((x, x - 1)) else None

  def main(args: Array[String]): Unit = {
    println(foldRight(productOp)(1 :: 10 :: 20 :: Nil))
    println(unfold(rangeOp)(10))
  }
}
