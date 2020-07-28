package tech.fixnthat

import exp.Rendering

object Fix2Type {

  // >>> convert
  //  rec f = f (rec f)
  // to types
  /*
    final case class Rec[F[_]](f    : F[Rec[F]])
  */

  // >>> rename to Fix & unfix

  final case class Fix[F[_]](unfix: F[Fix[F]])

  /*
  final case class Fix[F[_]](unfix: F[Fix[F]])
  */

  sealed trait FList[+H, +T]
  final case class FCons[H, T](h: H, t: T) extends FList[H, T]
  case object FNil extends FList[Nothing, Nothing]

  // >>> can't create a list using these directly - can't do without Fix
  // since can't specify type of tail
  //val nestedlist: FCons[Int, FCons[Int, FCons[Int, FNil]]](1, ???)

  def main(args: Array[String]): Unit = {

    val alist: Fix[FList[Int, ?]] = fcons(1, fcons(2, fnil))

    println(alist)
    //Rendering.of(alist, "2-alist")
  }

  def fnil[A] =
    Fix[FList[A, ?]](FNil)

  def fcons[A](h: A, t: Fix[FList[A, ?]]) =
    Fix[FList[A, ?]](FCons(h, t))

}
