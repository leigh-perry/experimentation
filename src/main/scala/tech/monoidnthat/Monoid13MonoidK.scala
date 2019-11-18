package tech.monoidnthat

import cats.data.Kleisli
import cats.instances.function._
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.instances.option._
import cats.instances.set._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.semigroupk._
import cats.{ Endo, FlatMap, MonoidK, SemigroupK }

object Monoid13MonoidK {

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3) |+| List(4, 2, 3))
    println(List(1, 2, 3) <+> List(4, 2, 3))
    println(Set(1, 2, 3) |+| Set(4, 2, 3))
    println(Set(1, 2, 3) <+> Set(4, 2, 3))

    println(Option(1) |+| Option(4))
    println(Option(1) <+> Option(4))

    println(Map("a" -> 1, "b" -> 2, "c" -> 3) |+| Map("a" -> 4, "b" -> 2, "c" -> 3))
    println(Map("a" -> 1, "b" -> 2, "c" -> 3) <+> Map("a" -> 4, "b" -> 2, "c" -> 3))

    println(List(1, 2, 3).foldMap(Option(_)))
    println(List(1, 2, 3).foldMapK(Option(_)))
    println("===============")

    ////

    val f1: Endo[Int] = _ * 2 // {i => println(s"f1($i)"); i * 2}
    val f2: Endo[Int] = _ + 3 // {i => println(s"f2($i)"); i + 3}
    val fcombined: Endo[Int] = f1 <+> f2
    println(fcombined(5))
    println("===============")

    ////

    //val xx = Plus[λ[α => (α => α)]]

    trait KleisliSemigroupK[F[_]] extends SemigroupK[λ[α => Kleisli[F, α, α]]] {
      implicit def F: SemigroupK[F]
      implicit def M: FlatMap[F]

      override def combineK[A](x: Kleisli[F, A, A], y: Kleisli[F, A, A]): Kleisli[F, A, A] =
        /*
        Kleisli(
          a => {
            val va: F[B] = x.run(a)
            println(va)
            val vb: F[B] = y.run(a)
            println(vb)
            F.combineK(va, vb)
          }
        )
         */
        x compose y
    }

    trait KleisliMonoidK[F[_]] extends MonoidK[λ[α => Kleisli[F, α, α]]] with KleisliSemigroupK[F] {
      implicit def F: MonoidK[F]

      override def empty[A]: Kleisli[F, A, A] = Kleisli.liftF(F.empty[A])
    }

    def inst[F[_]](F0: MonoidK[F], M0: FlatMap[F]): MonoidK[λ[α => Kleisli[F, α, α]]] =
      new KleisliMonoidK[F] {
        override def F: MonoidK[F] = F0
        override def M: FlatMap[F] = M0
      }

    ////

    if (true) {
      val k1 = Kleisli[Option, Int, Int](i => Option(i / 4))
      val k2 = Kleisli[Option, Int, Int](i => Option(i * 2))
      val k3 = Kleisli[Option, Int, Int](i => Option(i + 3))
      //val k1 = Kleisli[Option, Int, Int](i => Option({println(s"k1($i)"); i * 2}))
      //val k2 = Kleisli[Option, Int, Int](i => Option({println(s"k2($i)"); i + 3}))
      val kcombined: Kleisli[Option, Int, Int] = k1 <+> k2 <+> k3
      println(kcombined(5))

      val M = MonoidK[Option]
      val F = FlatMap[Option]
      val kcombined2a: Kleisli[Option, Int, Int] =
        inst(M, F).combineK(k1, inst(M, F).combineK(k2, k3))
      println(kcombined2a(5))
      val kcombined2b: Kleisli[Option, Int, Int] =
        inst(M, F).combineK(inst(M, F).combineK(k1, k2), k3)
      println(kcombined2b(5))

      println((k1 compose k2 compose k3).run(5))
      println("===============")
    }

    if (true) {
      val k1 = Kleisli[List, Int, Int](i => List(i / 4))
      val k2 = Kleisli[List, Int, Int](i => List(i * 2))
      val k3 = Kleisli[List, Int, Int](i => List(i + 3))
      val kcombined: Kleisli[List, Int, Int] = k1 <+> k2 <+> k3
      println(kcombined(5))

      val M = MonoidK[List]
      val F = FlatMap[List]
      val kcombined2a: Kleisli[List, Int, Int] =
        inst(M, F).combineK(k1, inst(M, F).combineK(k2, k3))
      println(kcombined2a(5))
      val kcombined2b: Kleisli[List, Int, Int] =
        inst(M, F).combineK(inst(M, F).combineK(k1, k2), k3)
      println(kcombined2b(5))

      println((k1 compose k2 compose k3).run(5))
      println("===============")
    }

    // TODO Category

  }
}
