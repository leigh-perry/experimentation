package tech.gentypes

import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}

object Poly6Laws {

  def main(args: Array[String]): Unit = {

    case class Transformable[A](gen: Gen[A], cogen: Cogen[A])
    object Transformable {
      implicit def transformable[A: Gen : Cogen]: Transformable[A] =
        Transformable(implicitly, implicitly)
    }

    ////

    implicit val genUnit = arbitrary[Unit]
    implicit val genBool = arbitrary[Boolean]
    implicit val genByte = arbitrary[Byte]
    implicit val genChar = arbitrary[Char]
    implicit val genShort = arbitrary[Short]
    implicit val genInt = arbitrary[Int]
    implicit val genLong = arbitrary[Long]

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

    def genMap(tB: TypeWith[Transformable]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType // first select a type
        coll <- genMultilevel(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.map(f)

    def genFlatMap(tB: TypeWith[Transformable]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType // first select a type
        coll <- genMultilevel(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.flatMap(
        a =>
          Coll.of(List.fill(5)(f(a)))
      )

    def genMultilevel(t: TypeWith[Transformable]): Gen[Coll[t.Type]] =
      Gen.delay(
        Gen.oneOf(
          genSimple(t),
          genFilter(t),
          genMap(t),
          genFlatMap(t),
        )
      )

    ////

    // functor identity law
    Prop.forAll(
      for {
        t <- genType // first select a type
        c <- genMultilevel(t)
      } yield c
    ) {
      c =>
        c === c.map(identity)
    }.check

    ////

    // functor associative law:
    //    F(fbc) ∘ F(fab)) = F(fbc ∘ fab)
    Prop.forAll(
      for {
        tA <- genType // first select a type A
        tB <- genType // first select a type B
        tC <- genType // first select a type C
        // cA <- genMultilevel(tA)
        // fab <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
        // fbc <- Gen.function1(tC.evidence.gen)(tB.evidence.cogen)
      } yield (tA, tB, tC)
    ) {
      case (tA: TypeWith[Transformable], tB: TypeWith[Transformable], tC: TypeWith[Transformable]) =>

        val cA: Coll[tA.Type] = genMultilevel(tA).sample.get
        val fab: tA.Type => tB.Type = Gen.function1(tB.evidence.gen)(tA.evidence.cogen).sample.get
        val fbc: tB.Type => tC.Type = Gen.function1(tC.evidence.gen)(tB.evidence.cogen).sample.get

        // import cats.syntax.show._
        // println(cA.map(fab).map(fbc).show)
        // println(cA.map(fbc.compose(fab)).show)
        // println("---------")

        cA.map(fab).map(fbc) === cA.map(fbc.compose(fab))
    }.check

  }

}
