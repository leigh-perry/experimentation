package exp

import scala.annotation.tailrec

object Trampoline {
  def main(args: Array[String]): Unit = {
    val result = runTrampoline(even2(999999999L))
    println(result)
  }

  def even2(n: Long): Bounce[Boolean] = {
    if (n == 0) {
      Done(true)
    } else {
      Call(() => odd2(n - 1))
    }
  }
  def odd2(n: Long): Bounce[Boolean] = {
    if (n == 0) {
      Done(false)
    } else {
      Call(() => even2(n - 1))
    }
  }

  sealed trait Bounce[A]
  case class Done[A](result: A) extends Bounce[A]
  case class Call[A](thunk: () => Bounce[A]) extends Bounce[A]

  @tailrec
  def runTrampoline[A](bounce: Bounce[A]): A = {
    bounce match {
      case Call(thunk) => runTrampoline(thunk())
      case Done(x) => x
    }
  }

}
