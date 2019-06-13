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
    //type TType = TypeWith[GenCogen]#Type
    trait TInfo {
      type A
      type B
      type C
      val cA: Coll[A]
      val fab: A => B
      val fbc: B => C
    }

    Prop.forAll(
      genType.flatMap[TInfo](
        tA =>
          genType.flatMap[TInfo](
            tB =>
              genType.flatMap[TInfo](
                tC =>
                  genMultilevel(tA).flatMap[TInfo](
                    ccA =>
                      Gen.function1[tA.Type, tB.Type](tB.evidence.gen)(tA.evidence.cogen).flatMap[TInfo](
                        cfab =>
                          Gen.function1[tB.Type, tC.Type](tC.evidence.gen)(tB.evidence.cogen)
                            .map(
                              cfbc =>
                                new TInfo {
                                  type A = tA.Type
                                  type B = tB.Type
                                  type C = tC.Type
                                  val cA: Coll[A] = ccA
                                  val fab: A => B = cfab
                                  val fbc: B => C = cfbc
                                }
                            )
                      )
                  )
              )
          )
      )
    ) {
      info =>
        import info._

        // import cats.syntax.show._
        // println(cA.map(fab).map(fbc).show)
        // println(cA.map(fbc.compose(fab)).show)
        // println("---------")

        // F(fbc) ∘ F(fab) = F(fbc ∘ fab)
        cA.map(fab).map(fbc) === cA.map(fbc.compose(fab))
    }.check

  }

}
