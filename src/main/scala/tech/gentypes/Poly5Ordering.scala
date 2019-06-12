package tech.gentypes

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}
import tech.gentypes.Temp.isSorted

object Poly5Ordering {

  def main(args: Array[String]): Unit = {

    case class OrderingGenCogen[A](gen: Gen[A], cogen: Cogen[A], ordering: Ordering[A])
    object OrderingGenCogen {
      implicit def orderingGenCogen[A: Gen : Cogen : Ordering]: OrderingGenCogen[A] =
        OrderingGenCogen(implicitly, implicitly, implicitly)
    }

    ////

    implicit val genByte = arbitrary[Byte]
    implicit val genChar = arbitrary[Char]
    implicit val genShort = arbitrary[Short]
    implicit val genInt = arbitrary[Int]
    implicit val genLong = arbitrary[Long]
    implicit val genString = Gen.alphaNumStr // arbitrary[String]

    def genOrderingType: Gen[TypeWith[OrderingGenCogen]] =
      Gen.oneOf(
        TypeWith[Byte, OrderingGenCogen],
        TypeWith[Char, OrderingGenCogen],
        TypeWith[Short, OrderingGenCogen],
        TypeWith[Int, OrderingGenCogen],
        TypeWith[Long, OrderingGenCogen],
        TypeWith[String, OrderingGenCogen],
      )

    def genSimple(t: TypeWith[OrderingGenCogen]): Gen[Coll[t.Type]] =
      for {
        n <- Gen.chooseNum(0, 10)
        l <- Gen.listOfN(n, t.evidence.gen)
      } yield Coll.of(l)

    ////

    def genSorted(t: TypeWith[OrderingGenCogen]): Gen[Coll[t.Type]] =
      genSimple(t)
        .map {
          coll =>
            Coll.of(coll.list.sorted(t.evidence.ordering))
        }

    Prop.forAll {
      for {
        t <- genOrderingType // first select a type
        coll <- genSorted(t)
      } yield coll
    } {
      c =>
        import cats.syntax.show._
        println(c.show)
        isSorted(c.list)
    }.check

    ////

    // ... can mix `OrderingGenCogen` and `GenCogen`
    // ...generate "sorted" Coll for a type that doesn't support Ordering

    // def genPseudosorted(t: TypeWith[GenCogen]): Gen[Coll[t.Type]] =
    //   for {
    //     tA <- genOrderingType
    //     coll <- genSorted(tA)
    //     f <- Gen.function1(t.evidence.gen)(tA.evidence.cogen)
    //   } yield coll.map(f)
    //
    // final case class Foo()
    //
    // implicit val genFoo = ???
    //
    // val tFoo = TypeWith[Foo, OrderingGenCogen]
    //
    // Prop.forAll(genPseudosorted(tFoo)) {
    //   c =>
    //     true // some test for sorted Coll[Foo]
    // }.check

  }

}
