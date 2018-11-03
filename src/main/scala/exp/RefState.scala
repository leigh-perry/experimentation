package exp

import cats.data.State
import cats.effect.concurrent.Ref
import cats.effect.{IO, Sync, Timer}
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

object RefState {
  sealed trait Command
  final case class Damage(n: Int) extends Command
  final case class Heal(n: Int) extends Command

  def update[S](f: S => S): State[S, Unit] =
    for {
      v <- State.get[S]
      nv = f(v)
      _ <- State.set(nv)
    } yield ()

  def health(c: Command): State[Int, Unit] =
    c match {
      case Damage(n) => update(_ - n)
      case Heal(n) => update(_ + n)
    }

  def report: State[Int, String] =
    State.get
      .map(h => s"Health=$h")

  val commands = List(Damage(1), Damage(5), Heal(2))

  val res: List[String] =
    commands.traverse {
      c =>
        health(c) >> report
    }.runA(10).value

//  def main(args: Array[String]): Unit = {
//    println(res)
//  }

  ////

  trait Counter[F[_]] {
    def increment: F[Unit]
    def get: F[Long]
    def reset: F[Unit]
  }

  object Counter {
    def create[F[_] : Sync]: F[Counter[F]] = {
      val initial: F[Ref[F, Long]] = Ref.of[F, Long](0)
      initial.map {
        ref =>
          new Counter[F] {
            override def increment: F[Unit] = ref.update(_ + 1)
            override def get: F[Long] = ref.get
            override def reset: F[Unit] = ref.set(0)
          }
      }
    }
  }

  import scala.concurrent.duration._

  def tick[F[_] : Sync](counter: Counter[F])(implicit timer: Timer[F]): F[Unit] =
    timer.sleep(1.second) >> counter.increment >> tick[F](counter)

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext

    implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
    val program =
      for {
        c <- Counter.create[IO]
        _ <- tick[IO](c)
      } yield ()

    program.unsafeRunSync()
  }
}
