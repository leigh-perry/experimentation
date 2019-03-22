package combinators

import cats.Show
import cats.instances.string._
import cats.syntax.contravariant._
import testsupport.TestSupport

object ContravariantExp
  extends TestSupport {
  def main(args: Array[String]): Unit = {

    val showInt: Show[Int] = Show[String].contramap(_.toString)

    showInt.show(1234)
      .shouldBe("1234")

    ()
  }
}
