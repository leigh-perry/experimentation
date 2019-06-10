package tech.gentypes

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop, Test}
import support.TestSupport

import scala.collection.immutable.List

object PolyProp1
  extends TestSupport {

  val genList: Gen[List[Int]] =
    Gen.listOf(arbitrary[Int])

  def main(args: Array[String]): Unit = {

    Prop.forAll(genList) {
      l: List[Int] =>
        val mapped = l.map(_ + 12)
        mapped.length.shouldBe(l.length)
    }.check(Test.Parameters.default.withMinSuccessfulTests(110))

    ////////////////////////////////////////////////////////////////////////////////
    // Generators
    ////////////////////////////////////////////////////////////////////////////////

    def genFrom[A](count: Gen[Int], g: Gen[A]): Gen[Coll[A]] =
      for {
        n <- count
        lst <- Gen.listOfN(n, g)
      } yield Coll(lst)

    ////////////////////////////////////////////////////////////////////////////////
    // Generating functions
    ////////////////////////////////////////////////////////////////////////////////

    // — Gen[A] generates A values
    // — Cogen[A] consumes A values
    // — to generate (A, B) we need Gen[A] and Gen[B]
    // — to generate A => B we need a Cogen[A] and Gen[B]
    // — use map and flatMap to transform Gen

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
      tp =>
        tp.shouldSatisfy(_.length == 0)
    }.check(Test.Parameters.default.withMinSuccessfulTests(120))

    val (n1, n2) =
      (0 until 10000)
        .map {
          _ =>
            val list = Gen.listOfN(100, arbitrary[Int]).sample.get
            val pred = intPredicate.sample.get
            list.filter(pred).length
        }.partition(n => n < 50)
    println(s"${n1.length} ${n2.length}")

    ////

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
      tp =>
        tp.length.shouldBe(10)
    }.check(Test.Parameters.default.withMinSuccessfulTests(130))

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
      tp =>
        tp.length <= 10
    }.check(Test.Parameters.default.withMinSuccessfulTests(140))
  }
}
