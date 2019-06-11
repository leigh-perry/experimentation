package tech.gentypes

import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}

object Poly4TransformableFlatMap {

  def main(args: Array[String]): Unit = {

    final case class Transformable[A](gen: Gen[A], cogen: Cogen[A])
    object Transformable {
      implicit def transformable[A: Gen : Cogen]: Transformable[A] =
        Transformable(implicitly, implicitly)
    }

    implicit val genUnit = arbitrary[Unit]
    implicit val genBool = arbitrary[Boolean]
    implicit val genByte = arbitrary[Byte]
    implicit val genChar = arbitrary[Char]
    implicit val genShort = arbitrary[Short]
    implicit val genInt = arbitrary[Int]
    implicit val genLong = arbitrary[Long]

    // if we have implicit Gen, Cogen, this works
    val testWith: TypeWith[Transformable] = TypeWith[Int, Transformable]
    val evGen: Gen[testWith.Type] = testWith.evidence.gen
    val evCogen: Cogen[testWith.Type] = testWith.evidence.cogen

    def genType: Gen[TypeWith[Transformable]] =
      Gen.oneOf(
        TypeWith[Unit, Transformable],
        TypeWith[Boolean, Transformable],
        TypeWith[Byte, Transformable],
        TypeWith[Char, Transformable],
        TypeWith[Short, Transformable],
        TypeWith[Int, Transformable],
        TypeWith[Long, Transformable],
      )

    ////

    def genSimple(t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
      for {
        n <- Gen.chooseNum(0, 10)
        l <- Gen.listOfN(n, t.evidence.gen)
      } yield Coll.of(l)

    def genFilter(t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
      for {
        coll <- genMultilevel(t)
        predicate <- Gen.function1(arbitrary[Boolean])(t.evidence.cogen)
      } yield coll.filter(predicate)

    def genDistinct[A](t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
      for {
        coll <- genMultilevel(t)
      } yield coll.distinct

    def genMap(tB: TypeWith[Transformable]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType
        coll <- genMultilevel(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.map(f)

    ////

    def genFlatMap(tB: TypeWith[Transformable]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType
        coll <- genMultilevel(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.flatMap(
        a =>
          Coll.of(List.fill(2)(f(a)))
      )

    def genMultilevel(t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
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
        t <- genType
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
