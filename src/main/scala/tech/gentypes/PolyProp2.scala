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

object PolyProp2 {

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

    case class Testing[A](gen: Gen[A], cogen: Cogen[A])
    object Testing {
      implicit def testing[A: Gen : Cogen]: Testing[A] =
        Testing(implicitly, implicitly)
    }

    implicit val genUnit = arbitrary[Unit]
    implicit val genBool = arbitrary[Boolean]
    implicit val genByte = arbitrary[Byte]
    implicit val genChar = arbitrary[Char]
    implicit val genShort = arbitrary[Short]
    implicit val genInt = arbitrary[Int]
    implicit val genLong = arbitrary[Long]

    // if we have implicit Gen, Cogen, this works
    val testWith: TypeWith[Testing] = TypeWith[Int, Testing]
    val evGen: Gen[testWith.Type] = testWith.evidence.gen
    val c: Cogen[testWith.Type] = testWith.evidence.cogen

    def genType: Gen[TypeWith[Testing]] =
      Gen.oneOf(
        TypeWith[Unit, Testing],
        TypeWith[Boolean, Testing],
        TypeWith[Byte, Testing],
        TypeWith[Char, Testing],
        TypeWith[Short, Testing],
        TypeWith[Int, Testing],
        TypeWith[Long, Testing],
      )

    ////

    def genFrom(t: TypeWith[Testing]): Gen[Coll[t.Type]] =
      for {
        n <- Gen.chooseNum(0, 10)
        lst <- Gen.listOfN(n, t.evidence.gen)
      } yield Coll(lst)

    def genFilter(t: TypeWith[Testing]): Gen[Coll[t.Type]] =
      for {
        coll <- genFrom(t)
        predicate <- Gen.function1(arbitrary[Boolean])(t.evidence.cogen)
      } yield coll.filter(predicate)

    def genMap(tB: TypeWith[Testing]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType
        coll <- genColl(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.map(f)

    def genColl(t: TypeWith[Testing]): Gen[Coll[t.Type]] =
      Gen.delay(
        Gen.oneOf(
          genFrom(t),
          genFilter(t),
          genMap(t),
        )
      )

    ////

    Prop.forAll(
      for {
        t <- genType
        p <- genColl(t)
      } yield p
    ) {
      tp =>
        println(tp.show)
        tp === tp.reverse.reverse
    }.check

    ////

    def genFlatMap(tB: TypeWith[Testing]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType
        coll <- genColl2(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.flatMap(
        a =>
          Coll(List.fill(2)(f(a)))
      )

    def genColl2(t: TypeWith[Testing]): Gen[Coll[t.Type]] =
      Gen.delay(
        Gen.oneOf(
          genFrom(t),
          genFilter(t),
          genMap(t),
          genFlatMap(t),
        )
      )

    ////

    Prop.forAll(
      for {
        t <- genType
        p <- genColl2(t)
      } yield p
    ) {
      tp =>
        println(tp.show)
        tp === tp.reverse.reverse
    }.check

  }

}
