package tech.gentypes

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}
import tech.gentypes.Temp.isSorted

object Poly5Ordering {

  def main(args: Array[String]): Unit = {

    case class OrderingTransformable[A](gen: Gen[A], cogen: Cogen[A], ordering: Ordering[A])
    object OrderingTransformable {
      implicit def orderedTransformable[A: Gen : Cogen : Ordering]: OrderingTransformable[A] =
        OrderingTransformable(implicitly, implicitly, implicitly)
    }

    ////

    implicit val genByte = arbitrary[Byte]
    implicit val genChar = arbitrary[Char]
    implicit val genShort = arbitrary[Short]
    implicit val genInt = arbitrary[Int]
    implicit val genLong = arbitrary[Long]
    implicit val genString = Gen.alphaNumStr // arbitrary[String]

    def genOrderingType: Gen[TypeWith[OrderingTransformable]] =
      Gen.oneOf(
        TypeWith[Byte, OrderingTransformable],
        TypeWith[Char, OrderingTransformable],
        TypeWith[Short, OrderingTransformable],
        TypeWith[Int, OrderingTransformable],
        TypeWith[Long, OrderingTransformable],
        TypeWith[String, OrderingTransformable],
      )

    def genSimple(t: TypeWith[OrderingTransformable]): Gen[Coll[t.Type]] =
      for {
        n <- Gen.chooseNum(0, 10)
        l <- Gen.listOfN(n, t.evidence.gen)
      } yield Coll.of(l)

    ////

    def genSorted(t: TypeWith[OrderingTransformable]): Gen[Coll[t.Type]] =
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

    // ... can mix `OrderingTransformable` and `Transformable`
    // ...generate "sorted" Coll for a type that doesn't support Ordering

    // def genPseudosorted(t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
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
    // val tFoo = TypeWith[Foo, OrderingTransformable]
    //
    // Prop.forAll(genPseudosorted(tFoo)) {
    //   c =>
    //     true // some test for sorted Coll[Foo]
    // }.check

  }

}
