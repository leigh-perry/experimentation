package tech.gentypes

import cats.syntax.eq._
import cats.syntax.show._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}
import support.TestSupport

object PolyProp4
  extends TestSupport {

  def main(args: Array[String]): Unit = {

    case class Testing[A](gen: Gen[A], cogen: Cogen[A])
    object Testing {
      implicit def testing[A: Gen : Cogen]: Testing[A] =
        Testing(implicitly, implicitly)
    }

    ////

    //    implicit def arbitraryGen[A](implicit arb: Arbitrary[A]): Gen[A] =
    //      arb.arbitrary

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
        p <- genColl(t)
      } yield p
    ) {
      tp =>
        tp === tp.map(identity)
    }.check

    ////

    Prop.forAll(
      for {
        tA <- genType
        tB <- genType
        tC <- genType
        pA <- genColl(tA)
        // fab <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
        // fbc <- Gen.function1(tC.evidence.gen)(tB.evidence.cogen)
      } yield (tA, tB, tC, pA)
    ) {
      case (tA: TypeWith[Testing], tB: TypeWith[Testing], tC: TypeWith[Testing], pA: Coll[TypeWith[Testing]#Type]) =>
        // def associative[A, B, C](fa: F[A], fab: A => B, fbc: B => C)(implicit FC: Equal[F[C]]): Boolean =
        //  FC.equal(map(map(fa)(fab))(fbc), map(fa)(fbc compose fab))

        val fab = Gen.function1(tB.evidence.gen)(tA.evidence.cogen).sample.get
        val fbc: tB.Type => tC.Type = Gen.function1(tC.evidence.gen)(tB.evidence.cogen).sample.get

        // TODO resolve this
        val fabTODO: TypeWith[Testing]#Type => tB.Type = fab.asInstanceOf[TypeWith[Testing]#Type => tB.Type]

        // println(pA.map(fabTODO).map(fbc).show)
        // println(pA.map(fbc.compose(fabTODO)).show)
        // println("---------")

        pA.map(fabTODO).map(fbc) === pA.map(fbc.compose(fabTODO))
    }.check

  }

}
