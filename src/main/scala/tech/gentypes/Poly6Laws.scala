package tech.gentypes

import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen, Prop}

object Poly6Laws {

  def main(args: Array[String]): Unit = {

    case class GenCogen[A](gen: Gen[A], cogen: Cogen[A])
    object GenCogen {
      implicit def genCogen[A: Gen : Cogen]: GenCogen[A] =
        GenCogen(implicitly, implicitly)
    }

    ////

    implicit val genUnit = arbitrary[Unit]
    implicit val genBool = arbitrary[Boolean]
    implicit val genByte = arbitrary[Byte]
    implicit val genChar = arbitrary[Char]
    implicit val genShort = arbitrary[Short]
    implicit val genInt = arbitrary[Int]
    implicit val genLong = arbitrary[Long]

    def genType: Gen[TypeWith[GenCogen]] =
      Gen.oneOf(
        TypeWith[Unit, GenCogen],
        TypeWith[Boolean, GenCogen],
        TypeWith[Byte, GenCogen],
        TypeWith[Char, GenCogen],
        TypeWith[Short, GenCogen],
        TypeWith[Int, GenCogen],
        TypeWith[Long, GenCogen],
      )

    ////

    def genSimple(t: TypeWith[GenCogen]): Gen[Coll[t.Type]] =
      for {
        n <- Gen.chooseNum(0, 10)
        l <- Gen.listOfN(n, t.evidence.gen)
      } yield Coll.of(l)

    def genFilter(t: TypeWith[GenCogen]): Gen[Coll[t.Type]] =
      for {
        coll <- genMultilevel(t)
        predicate <- Gen.function1(arbitrary[Boolean])(t.evidence.cogen)
      } yield coll.filter(predicate)

    def genMap(tB: TypeWith[GenCogen]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType // first select a type
        coll <- genMultilevel(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.map(f)

    def genFlatMap(tB: TypeWith[GenCogen]): Gen[Coll[tB.Type]] =
      for {
        tA <- genType // first select a type
        coll <- genMultilevel(tA)
        f <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
      } yield coll.flatMap(
        a =>
          Coll.of(List.fill(5)(f(a)))
      )

    def genMultilevel(t: TypeWith[GenCogen]): Gen[Coll[t.Type]] =
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
    //    F(fbc) ∘ F(fab) = F(fbc ∘ fab)
    type TType = TypeWith[GenCogen]#Type
    final case class TInfo(cA: Coll[TType], fab: TType => TType, fbc: TType => TType)

    Prop.forAll(
      for {
        tA <- genType // first select a type A
        tB <- genType // first select a type B
        tC <- genType // first select a type C
        cA <- genMultilevel(tA)
        fab <- Gen.function1(tB.evidence.gen)(tA.evidence.cogen)
        fbc <- Gen.function1(tC.evidence.gen)(tB.evidence.cogen)
        //   type Aux[A, Ev[_]] = TypeWith[Ev] {type Type = A}
      } yield TInfo(cA, fab.asInstanceOf[TType => TType], fbc.asInstanceOf[TType => TType])
    ) {
      info =>
        val TInfo(cA, fab, fbc) = info

        // import cats.syntax.show._
        // println(cA.map(fab).map(fbc).show)
        // println(cA.map(fbc.compose(fab)).show)
        // println("---------")

        // F(fbc) ∘ F(fab) = F(fbc ∘ fab)
        cA.map(fab).map(fbc) === cA.map(fbc.compose(fab))
    }.check

  }

}
