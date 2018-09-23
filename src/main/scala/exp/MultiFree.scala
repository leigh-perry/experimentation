package exp

import cats.data.Writer
import cats.effect.{ExitCode, IO, IOApp}
import cats.free.Free
import cats.instances.list._
import cats.syntax.functor._
import cats.{Id, ~>}
import iota.TListK.:::
import iota._
import org.slf4j.Logger


// val iota = "0.3.10"
// "io.frees" %% "iota-core" % Version.iota


sealed trait ExampleAlg[A]

object ExampleAlg {
  final case class ReadData(port: Int) extends ExampleAlg[String]
  final case class TransformData(data: String) extends ExampleAlg[String]
  final case class WriteData(port: Int, data: String) extends ExampleAlg[Unit]
}

////

sealed trait LogAlg[A]

object LogAlg {
  case class Info(message: String) extends LogAlg[Unit]
  case class Warn(message: String) extends LogAlg[Unit]
  case class Error(message: String) extends LogAlg[Unit]
}

////

// ../package.scala
package object stuff {

  type OverallAlg[A] = CopK[ExampleAlg ::: LogAlg ::: TNilK, A]

}

// TODO remove after creating proper package object
import exp.stuff._

////

// LogSyntax.scala
final class LogSyntaxOps[A](val a: LogAlg[A]) extends AnyVal {
  def alg: Free[OverallAlg, A] =
    Free.liftF(CopK.Inject[LogAlg, OverallAlg].inj(a))

}

trait ToLogSyntaxOps {
  implicit def `Ops for LogSyntax`[A](a: LogAlg[A]): LogSyntaxOps[A] =
    new LogSyntaxOps(a)
}

trait AllLogSyntaxOps
  extends ToLogSyntaxOps

object logsyntaxinstances
  extends AllLogSyntaxOps

////

// ExampleSyntax.scala
final class ExampleSyntaxOps[A](val a: ExampleAlg[A]) extends AnyVal {
  def alg: Free[OverallAlg, A] =
    Free.liftF(CopK.Inject[ExampleAlg, OverallAlg].inj(a))
}

trait ToExampleSyntaxOps {
  implicit def `Ops for ExampleSyntax`[A](a: ExampleAlg[A]): ExampleSyntaxOps[A] =
    new ExampleSyntaxOps(a)
}

trait AllExampleSyntaxOps
  extends ToExampleSyntaxOps

object examplesyntaxinstances
  extends AllExampleSyntaxOps

////

object AppMain
  extends IOApp
    with AllExampleSyntaxOps
    with AllLogSyntaxOps {

  override def run(args: List[String]): IO[ExitCode] =
    program.foldMap(interpreter)
      .as(ExitCode.Success)

  val program =
    for {
      _ <- LogAlg.Info("About to read").alg
      d <- ExampleAlg.ReadData(123).alg
      _ <- LogAlg.Info("About to transform").alg
      t <- ExampleAlg.TransformData(d).alg
      _ <- LogAlg.Info("About to write").alg
      result <- ExampleAlg.WriteData(789, t).alg
    } yield result

  ////

  private def exampleAlgInterpreter =
    new (ExampleAlg ~> IO) {
      override def apply[A](fa: ExampleAlg[A]): IO[A] = {
        fa match {
          case ExampleAlg.ReadData(port) => IO(s"abc inport $port")
          case ExampleAlg.TransformData(data) => IO(data.toUpperCase)
          case ExampleAlg.WriteData(port, data) => IO(println(s"outport $port [$data]"))
        }
      }
    }

  private def logInterpreter: LogAlg ~> IO =
    new (LogAlg ~> IO) {
      val log: Logger = org.slf4j.LoggerFactory.getLogger(getClass)
      override def apply[A](fa: LogAlg[A]): IO[A] = {
        fa match {
          case LogAlg.Info(s) => IO(log.info(s))
          case LogAlg.Warn(s) => IO(log.warn(s))
          case LogAlg.Error(s) => IO(log.error(s))
        }
      }
    }

  private val interpreter: OverallAlg ~> IO =
    CopK.FunctionK.of(exampleAlgInterpreter, logInterpreter)
}

////

object TestMain {
  def main(args: Array[String]): Unit = {
    val testInterpreter: OverallAlg ~> Id = CopK.FunctionK.of(testExampleAlgebraInterpreter, testLogInterpreter)
    AppMain.program.foldMap(testInterpreter)
  }

  private def testExampleAlgebraInterpreter =
    new (ExampleAlg ~> Id) {
      override def apply[A](fa: ExampleAlg[A]): Id[A] = {
        fa match {
          case ExampleAlg.ReadData(port) => s"abc inport $port"
          case ExampleAlg.TransformData(data) => data.toUpperCase
          case ExampleAlg.WriteData(port, data) => println(s"outport $port [$data]")
        }
      }
    }

  private def testLogInterpreter =
    new (LogAlg ~> Id) {
      override def apply[A](fa: LogAlg[A]): Id[A] = {
        fa match {
          case LogAlg.Info(s) => println(s)
          case LogAlg.Warn(s) => println(s)
          case LogAlg.Error(s) => println(s)
        }
      }
    }
}

////

trait FreeTestSupport {

  type OperationLogger[A] = Writer[List[Any], A]

  def ignore[A, B](b: B): OperationLogger[A] =
    Writer.tell(List.empty[Any])
      .map(_ => b)
      .asInstanceOf[OperationLogger[A]]

  def record[A, B](recordedValue: Any, result: B): OperationLogger[A] =
    Writer.tell(List[Any](recordedValue))
      .map(_ => result)
      .asInstanceOf[OperationLogger[A]]

}

object ExampleTest extends FreeTestSupport {
  // TODO change to test framework
  def main(args: Array[String]): Unit =
    test()

  def test(): Unit = {

    def inspector: OverallAlg ~> OperationLogger =
      new (OverallAlg ~> OperationLogger) {
        override def apply[A](fa: OverallAlg[A]): OperationLogger[A] =
          fa.value match {
            case o @ ExampleAlg.ReadData(port) =>
              record[A, String](o, "abcd")

            case o @ ExampleAlg.TransformData(data) =>
              record[A, String](o, data.reverse)

            case o @ ExampleAlg.WriteData(port, data) =>
              record[A, Unit](o, ())

            case LogAlg.Info(_) =>
              ignore[A, Unit](())
          }
      }

    val r: List[Any] = AppMain.program.foldMap(inspector).run._1
    println(r)
    assert(
      r == List[Any](
        ExampleAlg.ReadData(123),
        ExampleAlg.TransformData("abcd"),
        ExampleAlg.WriteData(789, "dcba")
      ),
      s"Test failed with result $r"
    )
  }

}


/*
trait LoggingSupport {
  def withLog[F[_] : Monad](tr: OverallAlg ~> F, log: Logging ~> F): OverallAlg ~> F =
    new (OverallAlg ~> F) {
      override def apply[A](fa: OverallAlg[A]): F[A] =
        log(info(s"OPERATION ${fa.value} starting")) >>
          tr(fa) >>= (r => log(info(s"OPERATION ${fa.value} completed\n  => $r")).as(r))
    }
}

object LoggingSupport
  extends LoggingSupport

////

trait LoggingSyntax {
  implicit def toLoggingOps[F[_] : Monad](tr: OverallAlg ~> F): LoggingOps[F] =
    LoggingOps(tr)
}

final case class LoggingOps[F[_]: Monad](tr: OverallAlg ~> F) {
  def withLog(log: Logging ~> F): OverallAlg ~> F =
    LoggingSupport.withLog(tr, log)
}

*/
