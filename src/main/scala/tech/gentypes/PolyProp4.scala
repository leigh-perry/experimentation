package tech.gentypes

import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}

object PolyProp4 {

  def main(args: Array[String]): Unit = {

    case class Testing[A](gen: Gen[A], cogen: Cogen[A])
    object Testing {
      implicit def testing[A: Gen : Cogen]: Testing[A] =
        Testing(implicitly, implicitly)
    }

    ////

    implicit val genUnit = arbitrary[Unit]
    implicit val genBool = arbitrary[Boolean]
    implicit val genByte = arbitrary[Byte]
    implicit val genChar = arbitrary[Char]
    implicit val genShort = arbitrary[Short]
    implicit val genInt = arbitrary[Int]
    implicit val genLong = arbitrary[Long]

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

    def genFlatMap(tB: TypeWith[Testing]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType
        coll <- genColl(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.flatMap(
        a =>
          Coll(List.fill(5)(f(a)))
      )

    def genColl(t: TypeWith[Testing]): Gen[Coll[t.Type]] =
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
        c <- genColl(t)
      } yield c
    ) {
      c =>
        c === c.map(identity)
    }.check

    ////

    Prop.forAll(
      for {
        tA <- genType
        tB <- genType
        tC <- genType
        // cA <- genColl(tA)
        // fab <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
        // fbc <- Gen.function1(tC.evidence.gen)(tB.evidence.cogen)
      } yield (tA, tB, tC)
    ) {
      case (tA: TypeWith[Testing], tB: TypeWith[Testing], tC: TypeWith[Testing]) =>

        val cA: Coll[tA.Type] = genColl(tA).sample.get
        val fab: tA.Type => tB.Type = Gen.function1(tB.evidence.gen)(tA.evidence.cogen).sample.get
        val fbc: tB.Type => tC.Type = Gen.function1(tC.evidence.gen)(tB.evidence.cogen).sample.get

        // import cats.syntax.show._
        //
        // println(cA.map(fab).map(fbc).show)
        // println(cA.map(fbc.compose(fab)).show)
        // println("---------")

        cA.map(fab).map(fbc) === cA.map(fbc.compose(fab))
    }.check

  }

}
