package tech.fixnthat

import tech.fixnthat.Fix2Type.{FCons, FList, FNil}

object Fix6HFix {

  // >>> type level Fix - eliminate the recursion in F[Fix[F]

  trait HfBase
  //    case class Fix[F[_]]              (unfix: F[Fix[F]])
  final case class HFix[F[_], G <: HfBase](unfix: F[G]) extends HfBase
  trait HfNil extends HfBase

  // >>>
  // factor out recursion at the type level
  // unlike Fix, not every element in the recursion have the same type.

  // >>> using previous fixed list (FList) can define HList

  // >>> Create a type-level list using HFix
  type ::[X, XS <: HfBase] = HFix[FList[X, ?], XS]
  type HNil = HFix[FList[Nothing, ?], HfNil]

  def main(args: Array[String]): Unit = {

    val hIntString: Int :: String :: HNil =
      hcons(1, hcons("bar", hnil))
    //Rendering.of(hIntString, "6-hIntString")
    println(hIntString)

    val hIntStringDouble: Int :: String :: Double :: HNil =
      hcons(1, hcons("a string", hcons(1.61803, hnil)))
    //Rendering.of(hIntStringDouble, "6-hIntStringDouble")
    println(hIntStringDouble)

  }

  def hcons[X, XS <: HfBase](x: X, xs: XS): X :: XS =
    HFix[FList[X, ?], XS](FCons(x, xs))

  val hnil: HNil = HFix[FList[Nothing, ?], HfNil](FNil)

}
