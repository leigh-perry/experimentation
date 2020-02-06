package naive

sealed trait Either[L, R] {
  def map[R2](f: R => R2): Either[L, R2] =
    this match {
      case Left(left) => Left(left)
      case Right(right) => Right(f(right))
    }

  def flatMap[R2](f: R => Either[L, R2]): Either[L, R2] =
    this match {
      case Left(left) => Left(left)
      case Right(right) => f(right)
    }
}

final case class Left[L, R](left: L) extends Either[L, R]
final case class Right[L, R](right: R) extends Either[L, R]

////

object EitherApp {
  def main(args: Array[String]): Unit = {
    val r1 =
      Right[Long, String]("a")
        .flatMap(
          a =>
            Right[Long, String]("b")
              .map(b => a + b)
        )

    println(r1)

    val r2 =
      for {
        a <- Right[Long, String]("a")
          b <- Right[Long, String]("b")
      } yield a + b

    println(r2)
  }
}
