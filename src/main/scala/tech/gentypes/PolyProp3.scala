package tech.gentypes

import cats.syntax.show._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}
import tech.gentypes.Temp.isSorted

object PolyProp3 {

  def main(args: Array[String]): Unit = {

    case class OrderedTesting[A](gen: Gen[A], cogen: Cogen[A], ordering: Ordering[A])
    object OrderedTesting {
      implicit def orderedOrderedTesting[A: Gen : Cogen : Ordering]: OrderedTesting[A] =
        OrderedTesting(implicitly, implicitly, implicitly)
    }

    ////

    implicit val genByte = arbitrary[Byte]
    implicit val genChar = arbitrary[Char]
    implicit val genShort = arbitrary[Short]
    implicit val genInt = arbitrary[Int]
    implicit val genLong = arbitrary[Long]
    implicit val genString = arbitrary[String]

    def genType: Gen[TypeWith[OrderedTesting]] =
      Gen.oneOf(
        TypeWith[Byte, OrderedTesting],
        TypeWith[Char, OrderedTesting],
        TypeWith[Short, OrderedTesting],
        TypeWith[Int, OrderedTesting],
        TypeWith[Long, OrderedTesting],
        TypeWith[String, OrderedTesting],
      )

    def genFrom(t: TypeWith[OrderedTesting]): Gen[Coll[t.Type]] =
      for {
        n <- Gen.chooseNum(0, 10)
        l <- Gen.listOfN(n, t.evidence.gen)
      } yield Coll.of(l)

    ////

    Prop.forAll(
      for {
        t0 <- genType
        coll <- genFrom(t0)
        ordering = t0.evidence.ordering
        c = Coll.of(coll.list.sorted(ordering))
      } yield c
    ) {
      c =>
        println(c.show)
        isSorted(c.list)
    }.check

  }

}
