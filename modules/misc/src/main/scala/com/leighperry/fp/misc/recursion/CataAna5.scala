package recursion

object CataAna5 {
  def foldRight[E, B](f: Option[(E, B)] => B): List[E] => B = {
    type F[P] = Option[(E, P)]
    type S = List[E]

    new (S => B) {
      kernel =>

      def step1: S => F[S] =
        _ match {
          case Nil => Option.empty[(E, S)]
          case ::(head, tl) => Option((head, tl))
        }

      def step2: F[S] => F[B] = // looks like `map`
        _ match {
          case Some((e, listE)) => Some((e, kernel(listE)))
          case None => None
        }

      def step3: F[B] => B =
        f

      override def apply(list: S): B =
        step3(step2(step1(list)))
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
