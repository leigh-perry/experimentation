package exp

import cats.data.Writer
import cats.effect.{ExitCode, IO, IOApp}
import cats.free.Free
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Id, Monad, ~>}
import iota.TListK.:::
import iota._


// val iota = "0.3.10"
// "io.frees" %% "iota-core" % Version.iota


sealed trait InputOutputAlg[A]

object InputOutputAlg {
  final case class ReadData(port: Int) extends InputOutputAlg[String]
  final case class WriteData(port: Int, data: String) extends InputOutputAlg[Unit]
}

////

sealed trait TransformerAlg[A]

object TransformerAlg {
  final case class TransformData(data: String) extends TransformerAlg[String]
}

////

// ../package.scala
package object stuff {

  type OverallAlg[A] = CopK[InputOutputAlg ::: TransformerAlg ::: TNilK, A]

}

// TODO remove after creating proper package object
import exp.stuff._

////

// TransformerSyntax.scala
final class TransformerSyntaxOps[A](val a: TransformerAlg[A]) extends AnyVal {
  def alg: Free[OverallAlg, A] =
    Free.liftF(CopK.Inject[TransformerAlg, OverallAlg].inj(a))

}

trait ToTransformerSyntaxOps {
  implicit def `Ops for TransformerSyntax`[A](a: TransformerAlg[A]): TransformerSyntaxOps[A] =
    new TransformerSyntaxOps(a)
}

trait AllTransformerSyntaxOps
  extends ToTransformerSyntaxOps

object transformersyntaxinstances
  extends AllTransformerSyntaxOps

////

// InputOutputSyntax.scala
final class InputOutputSyntaxOps[A](val a: InputOutputAlg[A]) extends AnyVal {
  def alg: Free[OverallAlg, A] =
    Free.liftF(CopK.Inject[InputOutputAlg, OverallAlg].inj(a))
}

trait ToInputOutputSyntaxOps {
  implicit def `Ops for InputOutputSyntax`[A](a: InputOutputAlg[A]): InputOutputSyntaxOps[A] =
    new InputOutputSyntaxOps(a)
}

trait AllInputOutputSyntaxOps
  extends ToInputOutputSyntaxOps

object inputOutputsyntaxinstances
  extends AllInputOutputSyntaxOps

////

object AppMain
  extends IOApp
    with AllInputOutputSyntaxOps
    with AllTransformerSyntaxOps
    with LogSyntax
    with LogSupport {

  override def run(args: List[String]): IO[ExitCode] =
    program.foldMap(interpreter.withLog(consoleLog))
      .as(ExitCode.Success)

  val program: Free[OverallAlg, Unit] =
    for {
      d <- InputOutputAlg.ReadData(123).alg
      t <- TransformerAlg.TransformData(d).alg
      result <- InputOutputAlg.WriteData(789, t).alg
    } yield result

  ////

  private def inputOutputAlgInterpreter =
    new (InputOutputAlg ~> IO) {
      override def apply[A](fa: InputOutputAlg[A]): IO[A] = {
        fa match {
          case InputOutputAlg.ReadData(port) => IO(s"abc inport $port")
          case InputOutputAlg.WriteData(port, data) => IO(println(s"outport $port [$data]"))
        }
      }
    }

  private def transformerInterpreter: TransformerAlg ~> IO =
    new (TransformerAlg ~> IO) {
      override def apply[A](fa: TransformerAlg[A]): IO[A] = {
        fa match {
          case TransformerAlg.TransformData(data) => IO(data.toUpperCase)
        }
      }
    }

  private val interpreter: OverallAlg ~> IO =
    CopK.FunctionK.of(inputOutputAlgInterpreter, transformerInterpreter)
}

////

object TestMain {
  def main(args: Array[String]): Unit = {
    val testInterpreter: OverallAlg ~> Id = CopK.FunctionK
      .of(testInputOutputAlgebraInterpreter, testTransformerInterpreter)
    AppMain.program.foldMap(testInterpreter)
  }

  private def testInputOutputAlgebraInterpreter =
    new (InputOutputAlg ~> Id) {
      override def apply[A](fa: InputOutputAlg[A]): Id[A] = {
        fa match {
          case InputOutputAlg.ReadData(port) => s"abc inport $port"
          case InputOutputAlg.WriteData(port, data) => println(s"outport $port [$data]")
        }
      }
    }

  private def testTransformerInterpreter =
    new (TransformerAlg ~> Id) {
      override def apply[A](fa: TransformerAlg[A]): Id[A] = {
        fa match {
          case TransformerAlg.TransformData(data) => data.toUpperCase
        }
      }
    }
}

////

trait FreeTestSupport {

  type Audit[A] = Writer[List[Any], A]

  def ignore[A, B](b: B): Audit[A] =
    Writer.tell(List.empty[Any])
      .map(_ => b)
      .asInstanceOf[Audit[A]]

  def record[A, B](recordedValue: Any, result: B): Audit[A] =
    Writer.tell(List[Any](recordedValue))
      .map(_ => result)
      .asInstanceOf[Audit[A]]

}

object InputOutputTest
  extends FreeTestSupport
    with LogSyntax
    with LogSupport {
  // TODO change to test framework of choice
  def main(args: Array[String]): Unit =
    test()

  def test(): Unit = {

    def inspector: OverallAlg ~> Audit =
      new (OverallAlg ~> Audit) {
        override def apply[A](fa: OverallAlg[A]): Audit[A] =
          fa.value match {
            case o @ InputOutputAlg.ReadData(_) =>
              record[A, String](o, "abcd")

            case TransformerAlg.TransformData(data) =>
              ignore[A, String](data.reverse)

            case o @ InputOutputAlg.WriteData(_, _) =>
              record[A, Unit](o, ())
          }
      }

    val r: List[Any] = AppMain.program.foldMap(inspector.withLog(consoleLog)).run._1
    println(r)
    assert(
      r == List[Any](
        InputOutputAlg.ReadData(123),
        InputOutputAlg.WriteData(789, "dcba")
      ),
      s"Test failed with result $r"
    )
  }

}

////

sealed trait LogAlg[A]

object LogAlg {
  case class Info(message: String) extends LogAlg[Unit]
  case class Warn(message: String) extends LogAlg[Unit]
  case class Error(message: String) extends LogAlg[Unit]
}

trait LogSupport {
  def withLog[F[_] : Monad](tr: OverallAlg ~> F, log: LogAlg ~> F): OverallAlg ~> F =
    new (OverallAlg ~> F) {
      override def apply[A](fa: OverallAlg[A]): F[A] =
        for {
          _ <- log(LogAlg.Info(s"OPERATION ${fa.value} starting"))
          r <- tr(fa)
          _ <- log(LogAlg.Info(s"OPERATION ${fa.value} completed\n  => $r"))
        } yield r
    }

  def logger[F[_] : Applicative](info: String => Unit, warn: String => Unit, error: String => Unit): LogAlg ~> F =
    new (LogAlg ~> F) {
      override def apply[A](fa: LogAlg[A]): F[A] =
        fa match {
          case LogAlg.Info(message) => info(message).pure[F]
          case LogAlg.Warn(message) => warn(message).pure[F]
          case LogAlg.Error(message) => error(message).pure[F]
        }
    }

  def slf4jLog[F[_] : Applicative]: LogAlg ~> F = {
    val slf = org.slf4j.LoggerFactory.getLogger(getClass)
    logger(slf.info, slf.warn, slf.error)
  }

  def consoleLog[F[_] : Applicative]: LogAlg ~> F =
    logger(println, println, println)

  def silentLog[F[_] : Applicative]: LogAlg ~> F =
    logger(_ => (), _ => (), _ => ())
}

object alllogsupport
  extends LogSupport

////

trait LogSyntax extends LogSupport {
  implicit class LogOps[F[_] : Monad](tr: OverallAlg ~> F) {
    def withLog(log: LogAlg ~> F): OverallAlg ~> F =
      alllogsupport.withLog(tr, log)
  }
}
