package tech.monoidnthat

import cats.Applicative
import cats.data.Nested
import cats.instances.list._
import cats.instances.option._

object Monoid07Composition {

  def main(args: Array[String]): Unit = {

    //// Composition of Applicative

    // (a) Nesting
    // (b) Product

    // (a) Nesting
    val nestedApplicative = Applicative[Nested[List, Option, *]]

    // (b) Product
    import cats.data.Tuple2K
    val tupledApplicative = Applicative[Tuple2K[List, Option, *]]

    val combinedApplicative =
      Applicative[Tuple2K[List, Nested[Nested[List, Option, *], Option, *], *]]
  }
}
