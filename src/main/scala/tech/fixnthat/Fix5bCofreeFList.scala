package tech.fixnthat

import cats.syntax.option._
import exp.Rendering
import tech.fixnthat.Fix2Type.Fix

object Fix5bCofreeFList {

  //final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

  // >>> remove recursion from definition in usual way:
  //  generalise Cofree to A and B, and replace tail with F[B]

  //final case class Cofree [F[_], A   ](head: A, tail: F[Cofree[F, A]])
  //final case class CofreeF[F[_], A, B](head: A, tail: F[B])

  // >>> rename CofreeF => EnvT, head => ask, tail => lower
  final case class EnvT[F[_], A, B](ask: A, lower: F[B])

  // >>> the idea of replacing the explicit recursive element with a polymorphic type is `pattern functor`
  // >>> EnvT is the pattern functor of Cofree

  type Cofree[F[_], A] = Fix[EnvT[F, A, ?]]

  def main(args: Array[String]): Unit = {
    val fconel1: Cofree[Option, Int] = cofreeNode(1, None)
    Rendering.of(fconel1, "5b-fconel1")

    if (false) {
      val fconel12 = cofreeNode(2, fconel1.some)
      Rendering.of(fconel12, "5b-fconel12")

      println(fconel12)
    }
  }

  private def cofreeNode(ask: Int, lower: Option[Fix[EnvT[Option, Int, ?]]]) =
    Fix[EnvT[Option, Int, ?]](EnvT(ask, lower))
}
