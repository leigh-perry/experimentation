package techtalk.applicatives

import cats.effect.IO
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global

object TestParallel {
  def main(args: Array[String]): Unit = {
    testIO()
    //    testParallelAsMonad()
  }

  ////

  def testParallelAsMonad() = {
    program(KVStore()).unsafeRunSync()
  }

  final case class KVStore() {
    def get(key: String): IO[String] = IO(slow(key.toUpperCase))
    def slow[A](i: A): A = {
      println(s"starting $i")
      Thread.sleep(1500)
      println(s"done $i")
      i
    }
  }

  def program(store: KVStore) =
    for {
      _ <- store.get("a")
      x <- (store.get("b"), store.get("c")).parMapN(_ |+| _)
      _ <- store.get(x)
    } yield x

  ////

  private def testIO() = {
    val result: IO[Int] = (delayed(1), delayed(2), delayed(3)).parMapN(_ + _ + _)
    println(result.unsafeRunSync())
  }

  def delayed[A](i: A): IO[A] =
    IO {
      println(s"starting $i")
      Thread.sleep(1500)
      println(s"done $i")
      i
    }
}

//  def <*>[A, B](fa : F[A]) : F[B]
//  def *> [B]   (fb : F[B]) : F[B]
//  def <* [B]   (fb : F[B]) : F[C]
//    IO.shift *>

///**
//  * Some types that form a Monad, are also capable of forming an Applicative
//  * that supports parallel composition.
//  * The Parallel type class allows us to represent this relationship.
//  */
//trait Parallel[M[_], F[_]] extends NonEmptyParallel[M, F] {
//  def applicative: Applicative[F]
//  def monad: Monad[M]
//}
//
//object X {
//  implicit def ioParallel(implicit timer: Timer[IO]): Parallel[IO, IO.Par] =
//    new Parallel[IO, IO.Par] {
//      final override val applicative: Applicative[IO.Par] = parApplicative(timer)
//
//      // ...
//    }
//
//  implicit def parApplicative(implicit timer: Timer[IO]): Applicative[IO.Par] =
//    new Applicative[IO.Par] {
//
//      import IO.Par.{unwrap, apply => par}
//
//      final override def pure[A](x: A): IO.Par[A] = par(IO.pure(x))
//      final override def map2[A, B, Z](fa: IO.Par[A], fb: IO.Par[B])(f: (A, B) => Z): IO.Par[Z] =
//        par(IOParMap(timer, unwrap(fa), unwrap(fb))(f))
//
//      // ...
//    }
//}
