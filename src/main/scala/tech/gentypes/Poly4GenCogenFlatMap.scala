package tech.gentypes

import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}

object Poly4GenCogenFlatMap {

  def main(args: Array[String]): Unit = {

    final case class GenCogen[A](gen: Gen[A], cogen: Cogen[A])
    object GenCogen {
      implicit def genCogen[A: Gen : Cogen]: GenCogen[A] =
        GenCogen(implicitly, implicitly)
    }

    implicit val genUnit = arbitrary[Unit]
    implicit val genBool = arbitrary[Boolean]
    implicit val genByte = arbitrary[Byte]
    implicit val genChar = arbitrary[Char]
    implicit val genShort = arbitrary[Short]
    implicit val genInt = arbitrary[Int]
    implicit val genLong = arbitrary[Long]

    // if we have implicit Gen, Cogen, this works
    val testWith: TypeWith[GenCogen] = TypeWith[Int, GenCogen]
    val evGen: Gen[testWith.Type] = testWith.evidence.gen
    val evCogen: Cogen[testWith.Type] = testWith.evidence.cogen

    def genType: Gen[TypeWith[GenCogen]] =
      Gen.oneOf(
        TypeWith[Unit, GenCogen],
        TypeWith[Boolean, GenCogen],
        TypeWith[Byte, GenCogen],
        TypeWith[Char, GenCogen],
        TypeWith[Short, GenCogen],
        TypeWith[Int, GenCogen],
        TypeWith[Long, GenCogen],
      )

    ////

    def genSimple(t: TypeWith[GenCogen]): Gen[Coll[t.Type]] =
      for {
        n <- Gen.chooseNum(0, 10)
        l <- Gen.listOfN(n, t.evidence.gen)
      } yield Coll.of(l)

    def genFilter(t: TypeWith[GenCogen]): Gen[Coll[t.Type]] =
      for {
        coll <- genMultilevel(t)
        predicate <- Gen.function1(arbitrary[Boolean])(t.evidence.cogen)
      } yield coll.filter(predicate)

    def genDistinct[A](t: TypeWith[GenCogen]): Gen[Coll[t.Type]] =
      for {
        coll <- genMultilevel(t)
      } yield coll.distinct

    def genMap(tB: TypeWith[GenCogen]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType // first select a type
        coll <- genMultilevel(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.map(f)

    ////

    def genFlatMap(tB: TypeWith[GenCogen]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType // first select a type
        coll <- genMultilevel(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.flatMap(
        a =>
          Coll.of(List.fill(2)(f(a)))
      )

    def genMultilevel(t: TypeWith[GenCogen]): Gen[Coll[t.Type]] =
      Gen.delay(
        Gen.oneOf(
          genSimple(t),
          genFilter(t),
          genDistinct(t),
          genMap(t),
          genFlatMap(t),
        )
      )

    ////

    Prop.forAll(
      for {
        t <- genType // first select a type
        c <- genMultilevel(t)
      } yield c
    ) {
      c =>
        //import cats.syntax.show._
        //println(c.show)
        c === c.reverse.reverse
    }.check

  }

}
