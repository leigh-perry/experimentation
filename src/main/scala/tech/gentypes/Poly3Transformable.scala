package tech.gentypes

import cats.syntax.eq._
import cats.syntax.show._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}

import scala.util.Random

sealed abstract class TypeWith[Ev[_]] {
  type Type
  def evidence: Ev[Type]
}

object TypeWith {
  type Aux[A, Ev[_]] = TypeWith[Ev] {type Type = A}

  def apply[A, Ev[_]](implicit eva: Ev[A]): Aux[A, Ev] =
    new TypeWith[Ev] {
      type Type = A
      def evidence: Ev[Type] = eva
    }
}

////

object Poly3Transformable {

  def main(args: Array[String]): Unit = {

    ////////////////////////////////////////////////////////////////////////////////
    // Generating types
    ////////////////////////////////////////////////////////////////////////////////

    val t0: TypeWith[Ordering] = TypeWith[Int, Ordering]

    // we don't know what t0.Type is, but we have an Ordering[t.Type]
    val o: Ordering[t0.Type] = t0.evidence

    // similarly for Gen
    implicit val stringGen = arbitrary[String]
    val t1: TypeWith[Gen] = TypeWith[String, Gen]
    val genT1: Gen[t1.Type] = t1.evidence

    ////

    val orderingInt = TypeWith[Int, Ordering]
    val orderingByte = TypeWith[Byte, Ordering]
    val orderingString = TypeWith[String, Ordering]

    // the vector only tracks the evidence type (Ordering),
    // not the specific value types (Byte, Int, etc.)
    val types: Vector[TypeWith[Ordering]] =
    Vector(orderingInt, orderingByte, orderingString)

    val randomType = Random.shuffle(types).head
    val randomOrdering: Ordering[randomType.Type] = randomType.evidence

    ////

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

    def genFrom(t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
      for {
        n <- Gen.chooseNum(0, 10)
        l <- Gen.listOfN(n, t.evidence.gen)
      } yield Coll.of(l)

    def genFilter(t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
      for {
        coll <- genColl(t)
        predicate <- Gen.function1(arbitrary[Boolean])(t.evidence.cogen)
      } yield coll.filter(predicate)

    def genDistinct[A](t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
      for {
        coll <- genColl(t)
      } yield coll.distinct

    def genMap(tB: TypeWith[Transformable]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType
        coll <- genColl(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.map(f)

    // ...specifies target type tB, genMap selects a source type tA to map to tB

    def genColl(t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
      Gen.delay(
        Gen.oneOf(
          genFrom(t),
          genFilter(t),
          genDistinct(t),
          genMap(t),
        )
      )

    ////

    Prop.forAll(
      for {
        t <- genType
        c <- genColl(t)
      } yield c
    ) {
      c =>
        println(c.show)
        c === c.reverse.reverse
    }.check

  }

}
