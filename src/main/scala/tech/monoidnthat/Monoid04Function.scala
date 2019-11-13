package tech.monoidnthat

import cats.implicits._

object Monoid04Function {

  def main(args: Array[String]): Unit = {

    import Monoid03Tuple._

    if (false) {
      val int2IntFunctions: List[Int => Int] =
        List(
          (i: Int) => i * i,
          (i: Int) => i * 5,
          (i: Int) => i + 6
        )
      (0 to 10).foreach {
        i =>
          val total: Int => Int = int2IntFunctions.foldMap(identity) // avoid List.fold()
          println(s"$i => ${total(i)}")
      }

      println("===============")
    }

    ////

    if (false) {
      val int2MaxFunctions: List[Int => Max] =
        List(
          (i: Int) => Max(i * i),
          (i: Int) => Max(i * 5),
          (i: Int) => Max(i + 6)
        )
      (0 to 10).foreach {
        i =>
          val bestf: Int => Max = int2MaxFunctions.foldMap(identity) // avoid List.fold()
          val best: Max = bestf(i)
          println(s"$i => ${best}")
      }

      println("===============")
    }

    ////

    if (false) {
      val int2MinFunctions: List[Int => Min] =
        List(
          (i: Int) => Min(i * i),
          (i: Int) => Min(i * 5),
          (i: Int) => Min(i + 6)
        )
      (0 to 10).foreach {
        i =>
          val bestf: Int => Min = int2MinFunctions.foldMap(identity) // avoid List.fold()
          val best: Min = bestf(i)
          println(s"$i => ${best}")
      }

      println("===============")
    }
  }
}
