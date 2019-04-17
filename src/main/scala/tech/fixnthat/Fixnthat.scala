package tech.fixnthat

import cats.arrow.Compose
import cats.data.{Nested, NonEmptyList}
import cats.free.Cofree
import exp.Rendering

/*
from workflowy...

analysis of fix point
https://youtu.be/IcDc-nJTlwI?t=725
haskell
Prelude> rec f = f (rec f)
Prelude> :t rec
rec :: (t -> t) -> t

Prelude> fac f n = if (n < 3) then n else  n * f (n - 1)
Prelude> :t fac
fac :: (Ord t, Num t) => (t -> t) -> t -> t

implements rec f = f (rec f) in scala
has to fully model A => A to avoid strictness stack overflow
https://youtu.be/IcDc-nJTlwI?t=950 implements in scala
then implements in scala as Scope[A]
get rid of Scope and just use [A] on the rec function
https://youtu.be/IcDc-nJTlwI?t=1297
mathematical explanation of fix...  x = f(x)
then converts to fix data type
reifies from the haskell definition
changes logic into data
f becomes F
rec becomes Rec
changes Rec to Fix and a to unfix
builds List and Tree
https://youtu.be/IcDc-nJTlwI?t=1877
says without fix it would be nested types
show that

https://jto.github.io/articles/typelevel-fix/
type level fix - HList
coproduct using Fix

mu, y combinator
https://stackoverflow.com/questions/45580858/what-is-the-difference-between-fix-mu-and-nu-in-ed-kmetts-recursion-scheme-pac
Y combinator & Mu
http://debasishg.blogspot.com/2011/07/datatype-generic-programming-in-scala.html
http://debasishg.blogspot.com/2012/01/learning-type-level-fixpoint-combinator.html
[MAYBE] https://youtu.be/oRLkb6mqvVM?t=3939 Kris Nuttycombe
case class HFix[F[_[_], _], I](unfix: F[HFix[F, ?], I])
https://youtu.be/D8LdznWynyw?t=89 Greg Pfeil
is Cofree a fixpoint operator?
case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
replace the recursive part
case class CofreeF[F[_], A, B](head: A, tail: F[B])
https://www.youtube.com/watch?v=9T8A89jgeTI Y combinator background
https://bartoszmilewski.com/2017/02/28/f-algebras/amp/?__twitter_impression=true category theory
 */

object Fixnthat {

  def factorial(f: Int => Int): Int => Int = {
    case 1 => 1
    case n => n * f(n - 1)
  }

  //Prelude> facexp n = if (n < 3) then n else n * facexp (n - 1)
  //Prelude> :t facexp
  //facexp :: (Ord t, Num t) => t -> t

  //Prelude> fac f n = if (n < 3) then n else n * f (n - 1)
  //Prelude> :t fac
  //fac :: (Ord t, Num t) => (t -> t) -> t -> t

  //Prelude> rec g = g (rec g)
  //Prelude> :t rec
  //rec :: (t -> t) -> t

  //Prelude> fixfac = rec fac
  //Prelude> :t fixfac
  //fixfac :: (Ord t, Num t) => t -> t

  //Prelude> :t fixfac 5
  //fixfac 5 :: (Ord t, Num t) => t

  // rec f = f (rec f)
  def recNaive[A, B]: (A => A) => A =
    f => f(recNaive(f))

  // change each A into (A => A)
  // add a => to the impl args and apply (a) at the end to resolve
  //  def rec[A]: ((A => A) => (A => A)) => (A => A) =
  //    f => a => f(rec(f))(a)
  //  def rec[A]: ((A => A) => A => A) => A => A =
  //    (f: (A => A) => A => A) => (a: A) => f(rec(f))(a)

  def rec[A]: ((A => A) => A => A) => A => A =
    f => a => f(rec(f))(a)

  //// type level

  // rec f = f (rec f) .... to types
  final case class Fix[F[_]](unfix: F[Fix[F]])

  sealed trait FList[+H, +T]
  final case class FCons[H, T](h: H, t: T) extends FList[H, T]
  case object FNil extends FList[Nothing, Nothing]
  //  trait FNil extends FList[Nothing, Nothing]
  //  case object FNil extends FNil

  //val nestedlist: FList[Int, Int] = FCons[Int, FCons[Int, FCons[Int, ...]]](1, FCons[Int, FCons[Int, FCons[Int, ...]]](2, FCons[Int, FCons[Int, FCons[Int, ...]]](3, FNil)))
  // can't do without Fix since can't specify type of tail

  def fnil[A] =
    Fix[FList[A, ?]](FNil)

  def fcons[A](h: A, t: Fix[FList[A, ?]]) =
    Fix[FList[A, ?]](FCons(h, t))

  val alist: Fix[FList[Int, ?]] = fcons(1, fcons(2, fnil))

  ////

  import cats.implicits._

  val f: Int => Int = (_ + 1)
  val g: Int => Int = (_ * 100)
  private val anthened: Int => Int = f >>> g
  val x1: Int = anthened(3)

  //private val composed: Int => Int = f <<< g
  private val compose: Compose[Function] = Compose[Function]
  private val composed: Int => Int = compose.compose[Int, Int, Int](_ + 100, _ + 1)
  val x2: Int = composed(3)

  ////

  // Detour into Compose / Nested

  //  >:t Compose
  //    Compose :: f (g a) -> Compose f g a

  // final case class Nested[F[_], G[_], A](value: F[G[A]])

  ////

  // Conventional
  type XList[A] = Option[NonEmptyList[A]]
  type XNonEmptyList[A] = (A, List[A])

  // https://twitter.com/jaspervdj/status/1113347183208583168
  //  type NonEmpty a = Fix (Compose ((,) a) Maybe)
  //  type List a = Fix (Compose Maybe ((,) a))
  type CNel[A] = Fix[Nested[(A, ?), Option, ?]] // (A, Option[x])
  type CList[A] = Fix[Nested[Option, (A, ?), ?]] // Option[(A, x)]

  // https://gist.github.com/jaspervdj/f43d93bf5abfa2af5e67b04612884199

  // ne1, ne2 :: NonEmpty Int
  // ne1 = Fix (Compose (1, Nothing))
  // ne2 = Fix (Compose (1, Just (Fix (Compose (2, Nothing)))))

  // l0, l1, l2 :: List Int
  // l0 = Fix (Compose Nothing)
  // l1 = Fix (Compose (Just (1, Fix (Compose Nothing))))
  // l2 = Fix (Compose (Just (1, Fix (Compose (Just (2, Fix (Compose Nothing)))))))

  ////

  // https://twitter.com/rob_rix/status/1112932607740862464
  // type NonEmpty a = Cofree Maybe a
  // final case class Cofree[S[_], A](head: A, tail: S[Cofree[S, A]]) {
  type QNelC[A] = Cofree[Option, A]

  ////

  def main(args: Array[String]): Unit = {
    //    val x =
    //      (0 to 100).foldLeft(1234.0) {
    //        (b, _) => Math.cos(b)
    //      }
    //    println(x)

    //    val fixed: Int => Int = recNaive(factorial)
    val fixed = rec(factorial)
    println(fixed(6))

    ////

    println(alist)
    Rendering.of(alist, "alist")

    ////

    // type QNel[A] = Fix[Nested[(A, ?), Option, ?]]

    val nelNil = nelCons((-1, None))
    Rendering.of(nelNil, "nelNil")

    val nel1 = nelCons((1, Some(nelNil)))
    Rendering.of(nel1, "nel1")

    val nel12 = nelCons((2, Some(nel1)))
    Rendering.of(nel12, "nel12")

    println(nel12)

    ////

    //    Rendering.animate(
    //      Animation.startWith(nelNil)
    //        .iterateWithIndex(1)((nel, i) => nelCons((i, Some(nel)))),
    //      "nelGrow"
    //    )

    ////

    // type QList[A] = Fix[Nested[Option, (A, ?), ?]]

    val listNil: Fix[Nested[Option, (Int, ?), ?]] = listCons(None)
    Rendering.of(listNil, "listNil")

    val list1 = listCons(Some((1, listNil)))
    Rendering.of(list1, "list1")

    val list12 = listCons(Some((2, list1)))
    Rendering.of(list12, "list12")

    println(list12)

    ////

    // Cofree

    final case class Cofree[S[_], A](head: A, tail: S[Cofree[S, A]])

    val conel1: Cofree[Option, Int] = Cofree[Option, Int](1, None)
    Rendering.of(conel1, "conel1")
    val conel12 = Cofree[Option, Int](1, Some(conel1))
    Rendering.of(conel12, "conel12")

    println(conel12)

    // https://youtu.be/D8LdznWynyw?t=146
    final case class CofreeF[S[_], A, B](head: A, tail: S[B])
    final case class EnvT[S[_], A, B](ask: A, lower: S[B])
    type FixCofree[S[_], A] = Fix[EnvT[S, A, ?]]

    ////

    // Higher kinded Fix
    trait HfBase
    final case class HFix[F[_], G <: HfBase](unfix: F[G]) extends HfBase
    trait INil extends HfBase

    // we have recursion at the type level
    // unlike Fix, not every element in the recursion have the same type.

    type HNil = HFix[FList[FList[Nothing, Nothing], ?], INil]
    type ::[X, XS <: HfBase] = HFix[FList[X, ?], XS]

    val hnil: HNil = HFix[FList[FList[Nothing, Nothing], ?], INil](FNil)

    def hcons[X, XS <: HfBase](x: X, xs: XS): X :: XS = HFix[FList[X, ?], XS](FCons(x, xs))

    val hIntString: Int :: String :: HNil = hcons(1, hcons("bar", hnil))
    //Rendering.of(hIntString, "hIntString")
    println(hIntString)

    val hIntStringInt: Int :: String :: Int :: HNil = hcons(1, hcons("a string", hcons(3, hnil)))
    //Rendering.of(hIntStringInt, "hIntStringInt")
    println(hIntStringInt)

    // HList is recursive at both the type and value level at the same time,
    // List is only recursive at the value level

    ////

    // Coproduct

    sealed trait Cocons[+H, +T]
    final case class Inl[+H, +T](head: H) extends Cocons[H, T]
    final case class Inr[+H, +T](tail: T) extends Cocons[H, T]

    type :+:[H, T <: HfBase] = HFix[Cocons[H, ?], T]

    trait Inject[C <: HfBase, I] {
      def apply(i: I): C
    }

    object Inject {
      def apply[C <: HfBase, I](implicit inject: Inject[C, I]): Inject[C, I] = inject

      implicit def tlInject[H, T <: HfBase, I](implicit tlInj: Inject[T, I]): Inject[H :+: T, I] =
        new Inject[H :+: T, I] {
          def apply(i: I): H :+: T = HFix(Inr(tlInj(i)))
        }

      implicit def hdInject[H, T <: HfBase]: Inject[H :+: T, H] =
        new Inject[H :+: T, H] {
          def apply(i: H): H :+: T = HFix(Inl(i))
        }
    }

    class MkCoproduct[C <: HfBase] {
      def apply[T](t: T)(implicit inj: Inject[C, T]): C = inj(t)
    }

    def Coproduct[C <: HfBase] = new MkCoproduct[C]


    val rrrr1 = Coproduct[Int :+: String :+: INil](1) // shouldBe HFix(Inl(1))
    //Rendering.of(rrrr1, "rrrr1")
    println(rrrr1)

    val rrrr2 = Coproduct[Int :+: String :+: INil]("bar") // shouldBe HFix(Inr(HFix(Inl("bar"))))
    //Rendering.of(rrrr2, "rrrr2")
    println(rrrr2)

  }

  ////

  def nelCons(entry: (Int, Option[Fix[Nested[Tuple2[Int, ?], Option, ?]]])) =
    nelFix(nelNested(entry))

  def nelNested[A](entry: (A, Option[Fix[Nested[(A, ?), Option, ?]]])) =
    Nested[(A, ?), Option, Fix[Nested[(A, ?), Option, ?]]](entry)

  def nelFix[A](tail: Nested[(A, ?), Option, Fix[Nested[(A, ?), Option, ?]]]) =
    Fix[Nested[(A, ?), Option, ?]](tail)

  ////

  def listTail[A](tail: Nested[Option, (A, ?), Fix[Nested[Option, (A, ?), ?]]]) =
    Fix[Nested[Option, (A, ?), ?]](tail)

  def listNested[A](opt: Option[(A, Fix[Nested[Option, (A, ?), ?]])]): Nested[Option, (A, ?), Fix[Nested[Option, (A, ?), ?]]] =
    Nested[Option, (A, ?), Fix[Nested[Option, (A, ?), ?]]](opt)

  def listCons[A](opt: Option[(A, Fix[Nested[Option, (A, ?), ?]])]): Fix[Nested[Option, Tuple2[A, ?], ?]] =
    listTail(listNested(opt))

}
