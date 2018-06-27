package techtalk.applicatives

import scalaz._
import Scalaz._

object TestSequential {
  def main(args: Array[String]): Unit = {
    def delayed[A](i: A): Option[A] =
      Option {
        println(s"starting $i")
        Thread.sleep(1500)
        println(s"done $i")
        i
      }

    //    val o3 =
    //      for {
    //        i1 <- delayed(1)
    //        i2 <- delayed(2)
    //      } yield i1 + i2
    //    println(o3)

    //    val o3 =
    //      (delayed(1) |@| delayed(2)) {
    //        (i1, i2) => i1 + i2
    //      }

    val os = Array(1, 2).par.map(delayed) // oooh errr
    val o3 =
      (os(0) |@| os(1)) {
        (i1, i2) => i1 + i2
      }
    println(o3)
  }
}

