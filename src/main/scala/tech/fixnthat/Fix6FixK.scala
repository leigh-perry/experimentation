package tech.fixnthat

import tech.fixnthat.Fix2Type.{FCons, FList, FNil}

object Fix6FixK {

  // https://jto.github.io/articles/typelevel-fix/

  // script: type level Fix
  trait HfBase
  //final case class Fix[F[_]]             (unfix: F[Fix[F]])
  final case class FixT[F[_], G <: HfBase](unfix: F[G]) extends HfBase
  trait INil extends HfBase

  // script:
  // factor out recursion at the type level
  // unlike Fix, not every element in the recursion have the same type.

  // script: using previous fixed list (FList) can define HList

  type HNil = FixT[FList[FList[Nothing, Nothing], ?], INil]
  type ::[X, XS <: HfBase] = FixT[FList[X, ?], XS]

  val hnil: HNil = FixT[FList[FList[Nothing, Nothing], ?], INil](FNil)

  def hcons[X, XS <: HfBase](x: X, xs: XS): X :: XS =
    FixT[FList[X, ?], XS](FCons(x, xs))

  def main(args: Array[String]): Unit = {

    // script:
    // the only difference between a List and a HList is the recursion scheme.
    // A HList is recursive at both the type and value level at the same time,
    // while a List is only recursive at the value level.
    // Apart from that, they are the same.
    val hIntString: Int :: String :: HNil = hcons(1, hcons("bar", hnil))
    //Rendering.of(hIntString, "hIntString")
    println(hIntString)

    val hIntStringInt: Int :: String :: Int :: HNil = hcons(1, hcons("a string", hcons(3, hnil)))
    //Rendering.of(hIntStringInt, "hIntStringInt")
    println(hIntStringInt)

  }

}
