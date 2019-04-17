package tech.apnthat

import cats.NonEmptyParallel
import cats.effect.{ContextShift, IO, Timer}
import cats.instances.string._
import cats.syntax.parallel._
import cats.syntax.semigroup._

object TestParallel {
  def main(args: Array[String]): Unit = {
    testParallelAsMonad()
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

  import scala.concurrent.ExecutionContext

  implicit val contextShifter: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

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
