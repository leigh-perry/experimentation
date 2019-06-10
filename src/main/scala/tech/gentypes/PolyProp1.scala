package tech.gentypes

import cats.instances.int._
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop, Test}

import scala.collection.immutable.List

object PolyProp1 {

  val genList: Gen[List[Int]] =
    Gen.listOf(arbitrary[Int])

  def main(args: Array[String]): Unit = {

    def genFrom[A](count: Gen[Int], g: Gen[A]): Gen[Coll[A]] =
      for {
        n <- count
        lst <- Gen.listOfN(n, g)
      } yield Coll(lst)

    def genFilter[A](gcoll: Gen[Coll[A]], gp: Gen[A => Boolean]): Gen[Coll[A]] =
      for {
        coll <- gcoll
        predicate <- gp
      } yield coll.filter(predicate)

    def genMap[A, B](gcoll: Gen[Coll[A]], gf: Gen[A => B]): Gen[Coll[B]] =
      for {
        coll <- gcoll
        f <- gf
      } yield coll.map(f)

    ////

    Prop.forAll(
      genMap(
        gcoll = genFrom(Gen.const(10), arbitrary[Int]),
        gf = Gen.function1(arbitrary[Int])(implicitly[Cogen[Int]])
      )
    ) {
      c =>
        c.length === 10
    }.check

    ////

    def genColl[A: Arbitrary : Cogen](count: Gen[Int], g: Gen[A]): Gen[Coll[A]] = {
      val gp: Gen[A => Boolean] = arbitrary[A => Boolean]
      val gf: Gen[A => A] = arbitrary[A => A]

      lazy val self: Gen[Coll[A]] =
        Gen.delay(
          Gen.oneOf(
            genFrom(count, g),
            genFilter(self, gp),
            genMap(self, gf)
          )
        )
      self
    }

    ////

    Prop.forAll(genColl(Gen.const(10), arbitrary[Int])) {
      c =>
        c.length <= 10
    }.check
  }
}
