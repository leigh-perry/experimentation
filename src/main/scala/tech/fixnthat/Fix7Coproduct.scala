package tech.fixnthat

import tech.fixnthat.Fix6HFix.{HFix, HfBase, HfNil}

object Fix7Coproduct {

  // Coproduct

  // ADT representing coproduct choices

  sealed trait Cocons[+H, +T]
  final case class First[+H, +T](head: H) extends Cocons[H, T]
  final case class Next[+H, +T](tail: T) extends Cocons[H, T]

  type :+:[H, T <: HfBase] = HFix[Cocons[H, ?], T]

  trait Inject[C <: HfBase, I] {
    def apply(i: I): C
  }

  object Inject {
    def apply[C <: HfBase, I](implicit ev: Inject[C, I]): Inject[C, I] = ev

    // Injecting first element
    implicit def headInject[H, T <: HfBase]: Inject[H :+: T, H] =
      new Inject[H :+: T, H] {
        def apply(value: H): H :+: T = {
          val injected: Cocons[H, T] = First(value)
          println("headInject: " + HFix(injected))
          HFix(injected)
        }
      }

    // Injecting subsequent elements
    implicit def tailInject[H, T <: HfBase, A](implicit tlInj: Inject[T, A]): Inject[H :+: T, A] =
      new Inject[H :+: T, A] {
        def apply(value: A): H :+: T = {
          val injected: Cocons[H, T] = Next(tlInj(value))
          println("tailInject: " + HFix(injected))
          HFix(injected)
        }
      }
  }

  class HCoproduct[C <: HfBase] {
    def apply[T](t: T)(implicit inj: Inject[C, T]): C =
      inj(t)
  }

  def coproduct[C <: HfBase]: HCoproduct[C] =
    new HCoproduct[C]

  def main(args: Array[String]): Unit = {

    val coInt = coproduct[Int :+: String :+: HfNil](1) // shouldBe HFix(Inl(1))
    println(coInt)

    val coString = coproduct[Int :+: String :+: HfNil]("bar") // shouldBe HFix(Inr(HFix(Inl("bar"))))
    println(coString)

    val coDouble = coproduct[Int :+: String :+: Double :+: HfNil](1.234) // shouldBe HFix(Inr(HFix(Inr(HFix(Inl(1.234))))))
    println(coDouble)

  }

}
