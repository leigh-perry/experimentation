package tech.fixnthat

import tech.fixnthat.Fix2Type.{FCons, FList, FNil}

object Fix6HFix {

  // script: type level Fix - eliminate the Fix in F[Fix[F]
  trait HfBase
  //final case class Fix[F[_]]            (unfix: F[Fix[F]])
  final case class HFix[F[_], G <: HfBase](unfix: F[G]) extends HfBase
  trait HfNil extends HfBase

  // script:
  // factor out recursion at the type level
  // unlike Fix, not every element in the recursion have the same type.

  // script: using previous fixed list (FList) can define HList

  type HNil = HFix[FList[Nothing, ?], HfNil]
  type ::[X, XS <: HfBase] = HFix[FList[X, ?], XS]

  val hnil: HNil = HFix[FList[Nothing, ?], HfNil](FNil)

  def hcons[X, XS <: HfBase](x: X, xs: XS): X :: XS =
    HFix[FList[X, ?], XS](FCons(x, xs))

  def main(args: Array[String]): Unit = {

    // script:
    // the only difference between a List and a HList is the recursion scheme.
    // A HList is recursive at both the type and value level at the same time,
    // while a List is only recursive at the value level.
    // Apart from that, they are the same.
    val hIntString: Int :: String :: HNil = hcons(1, hcons("bar", hnil))
    //Rendering.of(hIntString, "hIntString")
    println(hIntString)

    val hIntStringInt: Int :: String :: Double :: HNil = hcons(1, hcons("a string", hcons(1.61803, hnil)))
    //Rendering.of(hIntStringInt, "hIntStringInt")
    println(hIntStringInt)

  }

}
