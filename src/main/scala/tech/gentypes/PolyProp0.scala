package tech.gentypes

import cats.instances.int._
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}

import scala.collection.immutable.List

object PolyProp0 {

  def main(args: Array[String]): Unit = {

    Prop.forAll {
      Gen.listOf(arbitrary[Int])
    } {
      l: List[Int] =>
        val mapped = l.map(_ + 12)
        mapped.sum === l.sum + mapped.length * 12
    }.check

    ////////////////////////////////////////////////////////////////////////////////
    // Generators
    ////////////////////////////////////////////////////////////////////////////////

    def genFrom[A](count: Gen[Int], g: Gen[A]): Gen[Coll[A]] =
      for {
        n <- count
        l <- Gen.listOfN(n, g)
      } yield Coll.of(l)

    ////////////////////////////////////////////////////////////////////////////////
    // Generating functions
    ////////////////////////////////////////////////////////////////////////////////

    // — Gen[A] generates A values
    // — Cogen[A] consumes A values
    // — to generate A => B we need a Cogen[A] and Gen[B]

    val intPredicate: Gen[Int => Boolean] =
      Gen.function1(arbitrary[Boolean])(implicitly[Cogen[Int]])

    val (l1, l2) =
      (0 until 100000)
        .map(_ => intPredicate.sample.get(10))
        .partition(identity)
    println(s"${l1.length} ${l2.length}")

    ////

    def genFilter[A](gcoll: Gen[Coll[A]], gp: Gen[A => Boolean]): Gen[Coll[A]] =
      for {
        coll <- gcoll
        predicate <- gp
      } yield coll.filter(predicate)

    ////

    Prop.forAll(
      genFilter(
        gcoll = genFrom(Gen.choose(0, 20), arbitrary[String]),
        gp = (_: String) => false
      )
    ) {
      _.length === 0
    }.check

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
}
