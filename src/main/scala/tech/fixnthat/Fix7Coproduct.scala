package tech.fixnthat

import tech.fixnthat.Fix6HFix.{HFix, HfBase, HfNil}

object Fix7Coproduct {

  // Coproduct

  // TODO up to here

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


  val rrrr1 = Coproduct[Int :+: String :+: HfNil](1) // shouldBe HFix(Inl(1))
  //Rendering.of(rrrr1, "rrrr1")
  println(rrrr1)

  val rrrr2 = Coproduct[Int :+: String :+: HfNil]("bar") // shouldBe HFix(Inr(HFix(Inl("bar"))))
  //Rendering.of(rrrr2, "rrrr2")
  println(rrrr2)

}
