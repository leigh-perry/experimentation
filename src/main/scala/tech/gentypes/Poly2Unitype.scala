package tech.gentypes

import cats.instances.int._
import cats.syntax.eq._
import cats.syntax.show._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}

object Poly2Unitype {

  def main(args: Array[String]): Unit = {

    // tests for Coll

    def genSimple[A](count: Gen[Int], g: Gen[A]): Gen[Coll[A]] =
      for {
        n <- count
        l <- Gen.listOfN(n, g)
      } yield Coll.of(l)

    def genFilter[A](gColl: Gen[Coll[A]], gPred: Gen[A => Boolean]): Gen[Coll[A]] =
      for {
        coll <- gColl
        predicate <- gPred
      } yield coll.filter(predicate)

    def genDistinct[A](gColl: Gen[Coll[A]]): Gen[Coll[A]] =
      gColl.map(_.distinct)

    def genMap[A, B](gColl: Gen[Coll[A]], gf: Gen[A => B]): Gen[Coll[B]] =
      for {
        coll <- gColl
        f <- gf
      } yield coll.map(f)

    ////

    if (false) {
      Prop.forAll(
        genMap(
          gColl = genSimple(Gen.const(10), arbitrary[Int]),
          gf = arbitrary[Int => Long]
        )
      ) {
        c =>
          //println(c.list)
          c.length === 10
      }.check
    }

    ////

    if (false) {

      // generate multiple levels of map, filter etc

      def genMultilevel[A: Arbitrary : Cogen](count: Gen[Int], g: Gen[A]): Gen[Coll[A]] = {
        lazy val self: Gen[Coll[A]] =
          Gen.delay(
            Gen.oneOf(
              genSimple(count, g),
              genFilter(self, arbitrary[A => Boolean]),
              genMap(self, arbitrary[A => A]),
              genDistinct(self),
            )
          )
        self
      }

      ////

      Prop.forAll(genMultilevel(Gen.const(10), arbitrary[Int])) {
        c =>
          println(c.show)
          c.length <= 10
      }.check
    }
  }
}
