package tech.gentypes

import cats.instances.int._
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Prop}

import scala.collection.immutable.List

object Poly1GenFunction {

  def main(args: Array[String]): Unit = {

    Prop.forAll {
      Gen.listOf(arbitrary[Int])
    } {
      list: List[Int] =>
        val mapped = list.map(_ + 12)
        mapped.sum === list.sum + mapped.length * 12
    }.check

    ////

    def genSimple[A](count: Gen[Int], genA: Gen[A]): Gen[Coll[A]] =
      for {
        n <- count
        l <- Gen.listOfN(n, genA)
      } yield Coll.of(l)

    ////

    // Generating functions - how?

    if (false) {

      final case class Seed()

      abstract case class Gen[A](run: Seed => (A, Seed)) {
        def map[B](f: A => B): Gen[B]
        def flatMap[B](f: A => Gen[B]): Gen[B]
      }

      // Gen[A] generates A values
      // Cogen[A] consumes A values

      // seed influenced by both seed in and A value consumed
      //    reseeding with input value
      abstract case class Cogen[A](perturb: (A, Seed) => Seed) {
        def contramap[S](f: S => A): Cogen[S]
      }

      object Cogen {
        def apply[A](implicit F: Cogen[A]): Cogen[A] = F
      }

      ///

      implicit lazy val cogenLong: Cogen[Long] = ???

      implicit lazy val cogenBoolean: Cogen[Boolean] =
        Cogen[Long]
          .contramap(b => if (b) 1L else 0L)

      implicit lazy val cogenByte: Cogen[Byte] =
        Cogen[Long]
          .contramap(_.toLong)

      implicit lazy val cogenShort: Cogen[Short] =
        Cogen[Long]
          .contramap(_.toLong)

      implicit lazy val cogenChar: Cogen[Char] =
        Cogen[Long]
          .contramap(_.toLong)

      implicit lazy val cogenInt: Cogen[Int] =
        Cogen[Long]
          .contramap(_.toLong)

      // ...etc


      // eg, to generate function Long => Boolean, need a Cogen[Long] and Gen[Boolean]

      def combine(seed0: Seed, cogen: Cogen[Long], gen: Gen[Boolean], n: Long): Boolean = {
        val seed1 = cogen.perturb(n, seed0)
        gen.run(seed1)._1
      }

      def combineCurried(seed0: Seed, cogen: Cogen[Long], gen: Gen[Boolean]): Long => Boolean =
        n => {
          val seed1 = cogen.perturb(n, seed0)
          gen.run(seed1)._1
        }


      def createFunction[A, B](seed0: Seed, cogen: Cogen[A], gen: Gen[B]): A => B =
        n => {
          val seed1 = cogen.perturb(n, seed0)
          gen.run(seed1)._1
        }

      // wrap in Gen[A => B] and propagate gen's seed value
    }

    val intPredicate: Gen[Int => Boolean] =
      arbitrary[Int => Boolean] //Gen.function1(arbitrary[Boolean])(implicitly[Cogen[Int]])

    if (false) {
      val (l1, l2) =
        (0 until 10000)
          .map(_ => intPredicate.sample.get(10))
          .partition(identity)
      println(s"${l1.length} ${l2.length}")

      val (n1, n2) =
        (0 until 10000)
          .map {
            _ =>
              val list = Gen.listOfN(100, arbitrary[Int]).sample.get
              val pred = intPredicate.sample.get
              list.filter(pred).length
          }.partition(n => n < 50)
      println(s"${n1.length} ${n2.length}")
    }

    ////

    // generate filtered Coll

    def genFilter[A](gColl: Gen[Coll[A]], gPred: Gen[A => Boolean]): Gen[Coll[A]] =
      for {
        coll <- gColl
        predicate <- gPred
      } yield coll.filter(predicate)

    ////

    if (false) {
      Prop.forAll(
        genFilter(
          gColl = genSimple(Gen.choose(0, 20), arbitrary[String]),
          gPred = Gen.const((_: String) => false)
        )
      ) {
        _.length === 0
      }.check
    }

    ////

    if (false) {
      Prop.forAll(arbitrary[List[Int] => List[Byte]]) {
        l =>
          println(l(List(1, 2, 3)))
          true
      }.check
    }
  }
}
