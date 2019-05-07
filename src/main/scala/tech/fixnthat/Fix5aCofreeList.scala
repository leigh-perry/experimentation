package tech.fixnthat

import exp.Rendering

object Fix5aCofreeList {

  final case class Cofree[S[_], A](head: A, tail: S[Cofree[S, A]])

  // https://twitter.com/rob_rix/status/1112932607740862464
  // type NonEmpty a = Cofree Maybe a
  type CofreeNel[A] = Cofree[Option, A]

  def main(args: Array[String]): Unit = {
    val conel1: Cofree[Option, Int] = Cofree[Option, Int](1, None)
    Rendering.of(conel1, "conel1")

    val conel12 = Cofree[Option, Int](1, Some(conel1))
    Rendering.of(conel12, "conel12")

    println(conel12)
  }
}
