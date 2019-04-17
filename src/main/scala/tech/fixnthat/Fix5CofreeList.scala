package tech.fixnthat

import cats.data.{Nested, NonEmptyList}
import cats.free.Cofree
import exp.Rendering
import tech.fixnthat.Fix2Type.{FCons, FList, FNil, Fix}

object Fix5CofreeList {

  // https://twitter.com/rob_rix/status/1112932607740862464
  // type NonEmpty a = Cofree Maybe a
  // final case class Cofree[S[_], A](head: A, tail: S[Cofree[S, A]]) {
  type QNelC[A] = Cofree[Option, A]

  ////````

  def main(args: Array[String]): Unit = {

    // Cofree

    final case class Cofree[S[_], A](head: A, tail: S[Cofree[S, A]])

    val conel1: Cofree[Option, Int] = Cofree[Option, Int](1, None)
    Rendering.of(conel1, "conel1")
    val conel12 = Cofree[Option, Int](1, Some(conel1))
    Rendering.of(conel12, "conel12")

    println(conel12)

    // https://youtu.be/D8LdznWynyw?t=146
    final case class CofreeF[S[_], A, B](head: A, tail: S[B])
    final case class EnvT[S[_], A, B](ask: A, lower: S[B])
    type FixCofree[S[_], A] = Fix[EnvT[S, A, ?]]

    ////```

    // Higher kinded Fix
    trait HfBase
    final case class HFix[F[_], G <: HfBase](unfix: F[G]) extends HfBase
    trait INil extends HfBase

    // we have recursion at the type level
    // unlike Fix, not every element in the recursion have the same type.

    type HNil = HFix[FList[FList[Nothing, Nothing], ?], INil]
    type ::[X, XS <: HfBase] = HFix[FList[X, ?], XS]

    val hnil: HNil = HFix[FList[FList[Nothing, Nothing], ?], INil](FNil)

    def hcons[X, XS <: HfBase](x: X, xs: XS): X :: XS = HFix[FList[X, ?], XS](FCons(x, xs))

    val hIntString: Int :: String :: HNil = hcons(1, hcons("bar", hnil))
    //Rendering.of(hIntString, "hIntString")
    println(hIntString)

    val hIntStringInt: Int :: String :: Int :: HNil = hcons(1, hcons("a string", hcons(3, hnil)))
    //Rendering.of(hIntStringInt, "hIntStringInt")
    println(hIntStringInt)

    // HList is recursive at both the type and value level at the same time,
    // List is only recursive at the value level

    ////

    // Coproduct

    sealed trait Cocons[+H, +T]
    final case class Inl[+H, +T](head: H) extends Cocons[H, T]
    final case class Inr[+H, +T](tail: T) extends Cocons[H, T]

    type :+:[H, T <: HfBase] = HFix[Cocons[H, ?], T]

    trait Inject[C <: HfBase, I] {
      def apply(i: I): C
    }

    object Inject {
      def apply[C <: HfBase, I](implicit inject: Inject[C, I]): Inject[C, I] = inject

      implicit def tlInject[H, T <: HfBase, I](implicit tlInj: Inject[T, I]): Inject[H :+: T, I] =
        new Inject[H :+: T, I] {
          def apply(i: I): H :+: T = HFix(Inr(tlInj(i)))
        }

      implicit def hdInject[H, T <: HfBase]: Inject[H :+: T, H] =
        new Inject[H :+: T, H] {
          def apply(i: H): H :+: T = HFix(Inl(i))
        }
    }

    class MkCoproduct[C <: HfBase] {
      def apply[T](t: T)(implicit inj: Inject[C, T]): C = inj(t)
    }

    def Coproduct[C <: HfBase] = new MkCoproduct[C]


    val rrrr1 = Coproduct[Int :+: String :+: INil](1) // shouldBe HFix(Inl(1))
    //Rendering.of(rrrr1, "rrrr1")
    println(rrrr1)

    val rrrr2 = Coproduct[Int :+: String :+: INil]("bar") // shouldBe HFix(Inr(HFix(Inl("bar"))))
    //Rendering.of(rrrr2, "rrrr2")
    println(rrrr2)

  }

  ////

  def nelCons(entry: (Int, Option[Fix[Nested[Tuple2[Int, ?], Option, ?]]])) =
    nelFix(nelNested(entry))

  def nelNested[A](entry: (A, Option[Fix[Nested[(A, ?), Option, ?]]])) =
    Nested[(A, ?), Option, Fix[Nested[(A, ?), Option, ?]]](entry)

  def nelFix[A](tail: Nested[(A, ?), Option, Fix[Nested[(A, ?), Option, ?]]]) =
    Fix[Nested[(A, ?), Option, ?]](tail)

  ////

  def listTail[A](tail: Nested[Option, (A, ?), Fix[Nested[Option, (A, ?), ?]]]) =
    Fix[Nested[Option, (A, ?), ?]](tail)

  def listNested[A](opt: Option[(A, Fix[Nested[Option, (A, ?), ?]])]): Nested[Option, (A, ?), Fix[Nested[Option, (A, ?), ?]]] =
    Nested[Option, (A, ?), Fix[Nested[Option, (A, ?), ?]]](opt)

  def listCons[A](opt: Option[(A, Fix[Nested[Option, (A, ?), ?]])]): Fix[Nested[Option, Tuple2[A, ?], ?]] =
    listTail(listNested(opt))

}
