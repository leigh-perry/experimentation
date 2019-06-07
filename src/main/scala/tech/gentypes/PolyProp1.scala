package tech.gentypes

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop, Test}
import support.TestSupport

import scala.collection.immutable.List

final case class TypedPipe[+A] private(list: List[A]) {
  def map[B](f: A => B): TypedPipe[B] = TypedPipe(list.map(f))
  def flatMap[B](f: A => TypedPipe[B]): TypedPipe[B] = TypedPipe(list.flatMap(f andThen (_.list)))
  def filter(p: A => Boolean): TypedPipe[A] = TypedPipe(list.filter(p))
  def distinct = TypedPipe(list.distinct)

  //  — group
  //  — join
  def length = list.length
}

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

    def genFrom[A](count: Gen[Int], g: Gen[A]): Gen[TypedPipe[A]] =
      for {
        n <- count
        lst <- Gen.listOfN(n, g)
      } yield TypedPipe(lst)

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

    def genFilter[A](gpipe: Gen[TypedPipe[A]], gp: Gen[A => Boolean]): Gen[TypedPipe[A]] =
      for {
        pipe <- gpipe
        predicate <- gp
      } yield pipe.filter(predicate)

    ////

    Prop.forAll(
      genFilter(
        gpipe = genFrom(Gen.choose(0, 20), arbitrary[String]),
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

    def genMap[A, B](gpipe: Gen[TypedPipe[A]], gf: Gen[A => B]): Gen[TypedPipe[B]] =
      for {
        pipe <- gpipe
        f <- gf
      } yield pipe.map(f)

    ////

    Prop.forAll(
      genMap(
        gpipe = genFrom(Gen.const(10), arbitrary[Int]),
        gf = Gen.function1(arbitrary[Int])(implicitly[Cogen[Int]])
      )
    ) {
      tp =>
        tp.length.shouldBe(10)
    }.check(Test.Parameters.default.withMinSuccessfulTests(130))

    ////

    def genPipe[A: Cogen](count: Gen[Int], g: Gen[A]): Gen[TypedPipe[A]] = {
      val c: Cogen[A] = implicitly
      val gp: Gen[A => Boolean] = Gen.function1(arbitrary[Boolean])(c) // arbitrary[A => Boolean]
      val gf: Gen[A => A] = Gen.function1(g)(c) // arbitrary[A => A]

      lazy val self: Gen[TypedPipe[A]] =
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

    Prop.forAll(genPipe(Gen.const(10), arbitrary[Int])) {
      tp =>
        tp.length <= 10
    }.check(Test.Parameters.default.withMinSuccessfulTests(140))
  }
}
