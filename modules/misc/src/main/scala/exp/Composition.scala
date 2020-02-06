package exp

import cats.data.Kleisli
import cats.effect.IO
import cats.instances.list._

//import cats.instances.function._
//import cats.syntax.functor._

object Composition {
  val f1 = Kleisli[List, Int, Int](i => List.fill(i)(i))
  val f2 = Kleisli[List, Int, String](l => List.fill(l)(l).map(i => s"[$i]"))

  val kio1: Kleisli[IO, Int, List[Int]] = Kleisli[IO, Int, List[Int]](i => IO(List.fill(i)(i)))
  val kio2: Kleisli[IO, List[Int], List[String]] = Kleisli[IO, List[Int], List[String]](l => IO(l.map(i => s"[$i]")))

  def main(args: Array[String]): Unit = {
    val program1a = f1 andThen f2
    val program1 = program1a.map(i => s"{$i}")
    println(program1(3))

    val program2a: Kleisli[IO, Int, List[String]] = kio1 andThen kio2
    val program2: Kleisli[IO, Int, List[String]] = program2a.map(_.map(i => s"{$i}"))
    println(program2(3).unsafeRunSync())
  }
}
