package tech.fixnthat

import exp.Rendering

object Fix5aCofreeNel {

  final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

  // type NonEmpty a = Cofree Maybe a
  type CofreeNel[A] = Cofree[Option, A]

  def main(args: Array[String]): Unit = {
    val conel1: Cofree[Option, Int] =
      Cofree[Option, Int](1, None)
    Rendering.of(conel1, "5a-conel1")

    val conel12: Cofree[Option, Int] =
      Cofree[Option, Int](2, Some(conel1))
    Rendering.of(conel12, "5a-conel12")

    println(conel12)
  }
}
