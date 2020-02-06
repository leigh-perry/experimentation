object IntelliJBug {

  sealed trait Error

  final case class OptionT[F[_], A](value: F[Option[A]])

  type Result[T] = Either[Error, T]
  val v: Either[Error, Option[Int]] = Right(Some(1))
  val wrong: OptionT[Result, Int] = OptionT(v)
  val right: OptionT[Result, Int] = OptionT[Result, Int](v)
}
