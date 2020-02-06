package exp

import cats.Eval
import cats.free.Cofree

//final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

object CofreeStuff {

  class ByName[A](run: => A)

  val fibs: Cofree[ByName, Int] = {
    def unfold(prev1: Int, prev2: Int): Cofree[ByName, Int] =
      Cofree(prev1 + prev2, Eval.later(new ByName(unfold(prev2, prev1 + prev2))))

    unfold(0, 1)
  }
}
