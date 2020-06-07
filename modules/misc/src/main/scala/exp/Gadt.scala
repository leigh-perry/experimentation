package exp

object Gadt {

  // https://www.youtube.com/watch?v=tEQ8bTNYj5g (OCaml)
  sealed trait Witness[A]
  object Witness {
    final case object IntW extends Witness[Int]
    final case object BooleanW extends Witness[Boolean]
    final case class ListW[A]() extends Witness[List[A]]
  }

  sealed trait Peano
  object Peano {
    final case class Zero() extends Peano
    final case class Succ[A <: Peano]() extends Peano
  }

  import Peano._
  type TZero = Zero
  type TOne = Succ[TZero]
  type TTwo = Succ[TOne]

  sealed trait Vect[N, A]
  object Vect {
    final case class VNil[A]() extends Vect[Zero, A]
    final case class VCons[N <: Peano, A](head: A, tail: Vect[N, A]) extends Vect[Succ[N], A]
  }

  /*
  type nil = unit
  type ('h, 't) cons = 'h -> 't

  type 'ls hlist =
    | [] : nil hlist
    | (::) : 'h * 't hlist -> (('h, 't) cons) hlist

  let x : (int -> string -> (int -> int) -> unit) hlist =
    [1; "str"; ((+) 1)]
   */

  //type THNil = Unit
  //type THCons[H, T] = H => T

  sealed trait HList[LS]
  object HList {
    final case object HNil extends HList[Unit]
    final case class HCons[H, T](head: H, tail: HList[T]) extends HList[(H, HList[T]) /*=> HList[H => T]*/]
  }

  implicit class HListOps[H, T](tail: HList[T]) {
    def :::(head: H): HList[(H, HList[T]) /*=> HList[H => T]*/] =
      HList.HCons(head, tail)
  }

  import HList._
  val hlist = 3 ::: "asdf" ::: 4.5 ::: HNil

  def main(args: Array[String]): Unit =
    println(hlist)
}
