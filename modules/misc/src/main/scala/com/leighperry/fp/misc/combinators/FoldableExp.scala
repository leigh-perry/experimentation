package combinators

import cats.data.Const
import cats.effect.IO
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.instances.tuple._
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.{Eval, Foldable}
import support.{OptionalSupport, TestSupport}

object FoldableExp
  extends TestSupport
    with OptionalSupport {
  def main(args: Array[String]): Unit = {

    //    def sequence_(implicit F: Foldable[F], G: Applicative[G]): G[Unit]
    //    def foldK(implicit F: Foldable[F], G: MonoidK[G]): G[A]
    //    def foldl[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B
    //    def foldr[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B]
    //    def contains_(v: A)(implicit ev: Eq[A], F: Foldable[F]): Boolean
    //    def foldSmash(prefix: A, delim: A, suffix: A)(implicit A: Monoid[A], F: Foldable[F]): A
    //    def mkString_(prefix: String, delim: String, suffix: String)(implicit A: Show[A], F: Foldable[F]): String
    //    def collectFirstSomeM[G[_], B](f: A => G[Option[B]])(implicit F: Foldable[F], G: Monad[G]): G[Option[B]]
    //    def findM[G[_]](p: A => G[Boolean])(implicit F: Foldable[F], G: Monad[G]): G[Option[A]]
    //    def collectFold[M](f: PartialFunction[A, M])(implicit F: Foldable[F], M: Monoid[M]): M
    //    def collectSomeFold[M](f: A ⇒ Option[M])(implicit F: Foldable[F], M: Monoid[M]): M
    //    def mkString_(delim: String)(implicit A: Show[A], F: Foldable[F]): String
    //    def foldMapK[G[_], B](f: A => G[B])(implicit F: Foldable[F], G: MonoidK[G]): G[B]
    //    def partitionBifold[H[_, _], B, C](
    //    def partitionBifoldM[G[_], H[_, _], B, C](
    //    def partitionEitherM[G[_], B, C](
    //    def partitionBifold[H[_, _], A, B, C](fa: F[A])(f: A => H[B, C])(implicit A: Alternative[F],
    //    def partitionBifoldM[G[_], H[_, _], A, B, C](
    //    def partitionEitherM[G[_], A, B, C](fa: F[A])(f: A => G[Either[B, C]])(implicit A: Alternative[F],

    //    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
    //    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
    //    def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B]
    //    def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]]
    //    def reduceLeftOption[A](fa: F[A])(f: (A, A) => A): Option[A]
    //    def reduceRightOption[A](fa: F[A])(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]]
    //    def minimumOption[A](fa: F[A])(implicit A: Order[A]): Option[A]
    //    def maximumOption[A](fa: F[A])(implicit A: Order[A]): Option[A]
    //    def get[A](fa: F[A])(idx: Long): Option[A]
    //    def collectFirst[A, B](fa: F[A])(pf: PartialFunction[A, B]): Option[B]
    //    def collectFirstSome[A, B](fa: F[A])(f: A => Option[B]): Option[B]
    //    def fold[A](fa: F[A])(implicit A: Monoid[A]): A
    //    def combineAll[A: Monoid](fa: F[A]): A
    //    def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B
    //    def foldM[G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B]
    //    def foldMapM[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Monad[G], B: Monoid[B]): G[B]
    //    def traverse_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit]
    //    def sequence_[G[_]: Applicative, A](fga: F[G[A]]): G[Unit]
    //    def foldK[G[_], A](fga: F[G[A]])(implicit G: MonoidK[G]): G[A]
    //    def find[A](fa: F[A])(f: A => Boolean): Option[A]
    //    def existsM[G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean]
    //    def forallM[G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean]
    //    def toList[A](fa: F[A]): List[A]
    //    def partitionEither[A, B, C](fa: F[A])(f: A => Either[B, C])(implicit A: Alternative[F]): (F[B], F[C])
    //    def filter_[A](fa: F[A])(p: A => Boolean): List[A]
    //    def takeWhile_[A](fa: F[A])(p: A => Boolean): List[A]
    //    def dropWhile_[A](fa: F[A])(p: A => Boolean): List[A]
    //    def intercalate[A](fa: F[A], a: A)(implicit A: Monoid[A]): A
    //    def compose[G[_]: Foldable]: Foldable[λ[α => F[G[α]]]]

    List(1, 2, 3)
      .traverse_(_.some)
      .assertIs(().some)
    List(1, 2, 3)
      .traverse_(i => (i != 2).option(i))
      .assertIs(None)

    List(1.some, 2.some, 3.some)
      .sequence_
      .assertIs(().some)
    List(1.some, None, 3.some)
      .sequence_
      .assertIs(None)

    // TODO foldK

    List(1, 2, 3)
      .foldl(0)(_ + _)
      .assertIs(6)

    List(1, 2, 3)
      .foldr(Eval.now(0))((total, evalB) => evalB.map(_ + total))
      .value
      .assertIs(6)

    List(1, 2, 3)
      .contains_(2)
      .assertIs(true)

    List(1, 2, 3)
      .foldSmash(10, 2, 20)
      .assertIs(40)
    List("1", "2", "3")
      .foldSmash("(", "&", ")")
      .assertIs("(1&2&3)")

    List(1, 2, 3)
      .mkString_("(", "&", ")")
      .assertIs("(1&2&3)")
    List(1, 2, 3)
      .mkString_("&")
      .assertIs("1&2&3")

    def parseInt(s: String): Either[String, Int] =
      Either.catchOnly[NumberFormatException](s.toInt).leftMap(_.getMessage)

    val keys1 = List("1", "2", "4", "5")
    val map1 = Map(4 -> "Four", 5 -> "Five")
    keys1.collectFirstSomeM[Either[String, ?], String](parseInt(_).map(map1.get))
      .assertIs(Some("Four").asRight)

    val map2 = Map(6 -> "Six", 7 -> "Seven")
    keys1.collectFirstSomeM[Either[String, ?], String](parseInt(_).map(map2.get))
      .assertIs(None.asRight)

    val keys2 = List("1", "x", "4", "5")
    keys2.collectFirstSomeM[Either[String, ?], String](parseInt(_).map(map1.get))
      .assertIs("For input string: \"x\"".asLeft)

    val keys3 = List("1", "2", "4", "x")
    keys3.collectFirstSomeM[Either[String, ?], String](parseInt(_).map(map1.get))
      .assertIs(Some("Four").asRight)

    List(1, 2, 3)
      .findM(i => IO(i == 2))
      .unsafeRunSync()
      .assertIs(2.some)

    List(1, 2, 3)
      .collectFold {
        case n if n > 1 => n
      }
      .assertIs(5)

    List(1, 2, 3)
      .collectSomeFold(i => if (i > 1) i.some else None)
      .assertIs(5)

    // TODO   def foldMapK[G[_], B](f: A => G[B])(implicit F: Foldable[F], G: MonoidK[G]): G[B]

    List(1, 2, 3)
      .partitionBifold[Tuple2, Int, String](a => (a, s"value$a"))
      .assertIs((List(1, 2, 3), List("value1", "value2", "value3")))
    List(1, 2, 3)
      .partitionBifold(a => Const[Int, Nothing](a))
      .assertIs((List(1, 2, 3), List()))

    List(1, 2, 3)
      .partitionBifoldM(a => IO((a, s"value$a")))
      .unsafeRunSync()
      .assertIs((List(1, 2, 3), List("value1", "value2", "value3")))

    List(1, 2, 3)
      .partitionEitherM[IO, Int, String](a => IO(a.asLeft))
      .unsafeRunSync()
      .assertIs((List(1, 2, 3), List()))
    List(1, 2, 3)
      .partitionEitherM[IO, Int, String](a => IO(s"value$a".asRight))
      .unsafeRunSync()
      .assertIs((List(), List("value1", "value2", "value3")))

    ////

    Foldable[List]
      .foldMap(List(1, 2, 3))(_ * 10)
      .assertIs(60)

    Foldable[List]
      .foldM(List(1, 2, 3), 0)((b, a) => IO(b + a))
      .unsafeRunSync()
      .assertIs(6)

    Foldable[List]
      .foldMapM(List(1, 2, 3))(i => IO(i * 10))
      .unsafeRunSync()
      .assertIs(60)

    ()
  }
}
