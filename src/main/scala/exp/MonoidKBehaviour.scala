package exp

import cats.{Endo, SemigroupK}
import cats.data.Kleisli
import cats.implicits._

object MonoidKBehaviour {

  def main(args: Array[String]): Unit = {

    val x1 = SemigroupK[Kleisli[List, Int, *]]
    val x2 = SemigroupK[λ[⍺ => (⍺ => ⍺)]]

    val f1: Endo[Int] = i => i / 4
    val f2: Endo[Int] = i => i * 2
    val f3: Endo[Int] = i => i + 3
    println(s"function composition: ${(f1 compose f2 compose f3) (5)}")

    val fcombined: Endo[Int] = f1 <+> f2 <+> f3
    println(s"function SemigroupK: ${fcombined(5)}")

    val k1 = Kleisli[List, Int, Int](i => List(i / 4))
    val k2 = Kleisli[List, Int, Int](i => List(i * 2))
    val k3 = Kleisli[List, Int, Int](i => List(i + 3))

    println(s"Kleisli composition: ${(k1 compose k2 compose k3).run(5)}")

    val kcombined: Kleisli[List, Int, Int] = k1 <+> k2 <+> k3
    println(s"Kleisli SemigroupK: ${kcombined(5)}")
  }
}
