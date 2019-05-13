package tech.fixnthat

import cats.syntax.option._
import exp.Rendering
import tech.fixnthat.Fix2Type.Fix

object Fix5bCofreeFList {

  //final case class Cofree[S[_], A](head: A, tail: S[Cofree[S, A]])

  // https://youtu.be/D8LdznWynyw?t=146
  // https://github.com/sellout/recursion-scheme-talk/blob/master/fix-ing-your-types.org

  // >>> remove recursion from definition in usual way:
  //  generalise Cofree to A and B, and replace tail with B
  //final case class Cofree[F[_], A    ](head: A, tail: F[QCofree[F, A]])
  //final case class CofreeF[F[_], A, B](head: A, tail: F[B])

  // >>> rename CofreeF => EnvT, head => ask, tail => lower
  final case class EnvT[F[_], A, B](ask: A, lower: F[B])

  // >>> the idea of replacing the explicit recursive element with a polymorphic type is `pattern functor`
  // >>> EnvT is the pattern functor of Cofree

  type Cofree[F[_], A] = Fix[EnvT[F, A, ?]]

  def main(args: Array[String]): Unit = {
    val fconel1: Cofree[Option, Int] = cofreeNode(1, None)
    Rendering.of(fconel1, "fconel1")

    val fconel12 = cofreeNode(2, fconel1.some)
    Rendering.of(fconel12, "fconel12")

    println(fconel12)
  }

  private def cofreeNode(ask: Int, lower: Option[Fix[EnvT[Option, Int, ?]]]) =
    Fix[EnvT[Option, Int, ?]](EnvT(ask, lower))
}
