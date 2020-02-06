package exp

import cats.data.{EitherT, OptionT, State, StateT}
import cats.effect.IO
import cats.implicits._

object MonadStack {
  sealed trait Error
  object Error {
    final case class SomeError(message: String) extends Error
  }

  def main(args: Array[String]): Unit = {
    testIoEitherState()
    println("----")
    testIoEitherOption()
  }

  private def testIoEitherState() = {

    final case class AppState(i: Int)

    val eoi: Either[Error, State[AppState, Int]] = State.pure[AppState, Int](1).asRight[Error]
    val eoit2: StateT[Either[Error, ?], AppState, Int] = StateT.pure[Either[Error, ?], AppState, Int](1)

    val ieoi: IO[Either[Error, State[AppState, Int]]] = IO(eoi)
    val ieoit: EitherT[IO, Error, State[AppState, Int]] = EitherT(ieoi)

    val ieoitt1: StateT[EitherT[IO, Error, ?], AppState, Int] = StateT.pure[EitherT[IO, Error, ?], AppState, Int](1)
    val ieoitt2: StateT[EitherT[IO, Error, ?], AppState, Int] = StateT.pure[EitherT[IO, Error, ?], AppState, Int](2)
    val ieoitt3: StateT[EitherT[IO, Error, ?], AppState, Int] =
      StateT.liftF[EitherT[IO, Error, ?], AppState, Int](EitherT.leftT(Error.SomeError("asdf")))
    val ieoitt4: StateT[EitherT[IO, Error, ?], AppState, Int] = StateT.pure[EitherT[IO, Error, ?], AppState, Int](4)
    val e: Error = Error.SomeError("error")
    val ieoitt5: StateT[EitherT[IO, Error, ?], AppState, Int] = StateT.liftF(EitherT.leftT[IO, Int](e))

    val result124: StateT[EitherT[IO, Error, ?], AppState, Int] =
      for {
        i1 <- ieoitt1
        i2 <- ieoitt2
        i4 <- ieoitt4
      } yield i1 + i2 + i4
    println(result124.run(AppState(0)).value.unsafeRunSync())

    val result1234: StateT[EitherT[IO, Error, ?], AppState, Int] =
      for {
        i1 <- ieoitt1
        i2 <- ieoitt2
        i3 <- ieoitt3
        i4 <- ieoitt4
      } yield i1 + i2 + i3 + i4
    println(result1234.run(AppState(0)).value.unsafeRunSync())

    val result1245: StateT[EitherT[IO, Error, ?], AppState, Int] =
      for {
        i1 <- ieoitt1
        i2 <- ieoitt2
        i4 <- ieoitt4
        i5 <- ieoitt5
      } yield i1 + i2 + i4 + i5
    println(result1245.run(AppState(0)).value.unsafeRunSync())
  }

  private def testIoEitherOption() = {

    val eoi: Either[Error, Option[Int]] = 1.some.asRight[Error]
    val eoit1: OptionT[Either[Error, ?], Int] = OptionT(eoi)
    val eoit2: OptionT[Either[Error, ?], Int] = OptionT.liftF(1.asRight)

    val ieoi: IO[Either[Error, Option[Int]]] = IO(eoi)
    val ieoit: EitherT[IO, Error, Option[Int]] = EitherT(ieoi)

    val ieoitt1: OptionT[EitherT[IO, Error, ?], Int] = OptionT.some(1)
    // val x: EitherT[IO, Error, Option[Int]] = ieoitt1.value
    // val xx: IO[Either[Error, Option[Int]]] = x.value

    val ieoitt2: OptionT[EitherT[IO, Error, ?], Int] = OptionT.some(2)
    val ieoitt3: OptionT[EitherT[IO, Error, ?], Int] = OptionT.none
    val ieoitt4: OptionT[EitherT[IO, Error, ?], Int] = OptionT.some(4)
    val e: Error = Error.SomeError("error")
    val ieoitt5: OptionT[EitherT[IO, Error, ?], Int] = OptionT.liftF(EitherT.leftT[IO, Int](e))

    val result124: OptionT[EitherT[IO, Error, ?], Int] =
      for {
        i1 <- ieoitt1
        i2 <- ieoitt2
        i4 <- ieoitt4
      } yield i1 + i2 + i4
    println(result124.value.value.unsafeRunSync())

    val result1234: OptionT[EitherT[IO, Error, ?], Int] =
      for {
        i1 <- ieoitt1
        i2 <- ieoitt2
        i3 <- ieoitt3
        i4 <- ieoitt4
      } yield i1 + i2 + i3 + i4
    println(result1234.value.value.unsafeRunSync())

    val result1245: OptionT[EitherT[IO, Error, ?], Int] =
      for {
        i1 <- ieoitt1
        i2 <- ieoitt2
        i4 <- ieoitt4
        i5 <- ieoitt5
      } yield i1 + i2 + i4 + i5
    println(result1245.value.value.unsafeRunSync())
  }
}

