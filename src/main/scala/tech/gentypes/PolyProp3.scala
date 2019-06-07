package tech.gentypes

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop, Test}
import support.TestSupport
import tech.gentypes.PolyProp2.dump

object PolyProp3
  extends TestSupport {

  def main(args: Array[String]): Unit = {

    case class OrderedTesting[A](gen: Gen[A], cogen: Cogen[A], ordering: Ordering[A])
    object OrderedTesting {
      implicit def orderedOrderedTesting[A: Gen : Cogen : Ordering]: OrderedTesting[A] =
        OrderedTesting(implicitly, implicitly, implicitly)
    }

    ////

    implicit val genUnit = arbitrary[Unit]
    implicit val genBool = arbitrary[Boolean]
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
        TypeWith[String, OrderedTesting],
      )

    ////

    def genFrom(t: TypeWith[OrderedTesting]): Gen[TypedPipe[t.Type]] =
      for {
        n <- Gen.chooseNum(0, 10)
        lst <- Gen.listOfN(n, t.evidence.gen)
      } yield TypedPipe(lst)

    //    def genPipe(t: TypeWith[OrderedTesting]): Gen[TypedPipe[t.Type]] =
    //      Gen.delay(
    //        Gen.oneOf(
    //          genFrom(t),
    //          genSorted(t),
    //        )
    //      )

    ////

    Prop.forAll(
      for {
        t0 <- genType
        pipe <- genFrom(t0)
      } yield (TypedPipe(pipe.list.sorted(t0.evidence.ordering)), t0.evidence.ordering)
    ) {
      case (tp, ordering) =>
        println(dump(tp))
        tp.shouldSatisfy(_.list.sorted(ordering))
    }.check(Test.Parameters.default.withMinSuccessfulTests(120))

  }

}
