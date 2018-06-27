package techtalk.applicatives

import scalaz._
import Scalaz._

object TestValidation {
  def main(args: Array[String]): Unit = {

    sealed trait ErrorType
    object ErrorType {
      final case class ErrorMessage(message: String) extends ErrorType
      final case class ErrorCode(count: Int) extends ErrorType
    }

    import ErrorType._

    val vn0: ValidationNel[ErrorType, Int] = 0.successNel[ErrorType]
    val vn1: ValidationNel[ErrorType, Int] = (ErrorMessage("Ummmm"): ErrorType).failureNel[Int]
    val vn2: ValidationNel[ErrorType, Int] = (ErrorCode(500): ErrorType).failureNel[Int]

    val outcome: ValidationNel[ErrorType, Int] =
      (vn0 |@| vn1 |@| vn2) {
        (v0, v1, v2) => v0 + v1 + v2
      }
    println(outcome)
  }
}