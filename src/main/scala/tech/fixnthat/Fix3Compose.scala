package tech.fixnthat

import cats.data.Nested
import cats.syntax.functor._
import cats.syntax.option._
import cats.instances.list._
import cats.instances.option._

object Fix3Compose {

  // >>> detour into Compose / Nested
  //    nested functors compose

  //  Prelude>:t Compose
  //    Compose :: f (g a) -> Compose f g a

  // >>>
  //  in cats
  // final case class Nested[F[_], G[_], A](value: F[G[A]])

  def main(args: Array[String]): Unit = {
    val listOfOptions: Nested[List, Option, Int] = Nested(List(1.some, 2.some, None))
    println(listOfOptions.map(_ * 10))

    val optionalList = Nested(List(1, 2, 3).some)
    println(optionalList.map(_ * 10))

    println(Nested(List(Option.empty[Int])).map(_ * 10))
  }

}
