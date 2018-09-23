package exp

import scala.annotation.tailrec

sealed abstract class IOLite[A] {
  self =>

  def unsafePerformIO: A

  def map[B](f: A => B): IOLite[B] = {
    flatMap(a => IOLite.Pure(f(a)))
  }

  def flatMap[B](f: A => IOLite[B]): IOLite[B] = {
    this match {
      case c: IOLite.Compute[A] =>
        new IOLite.Compute[B] {
          type Start = c.Start

          // https://issues.scala-lang.org/browse/SI-9931
          val start: () => IOLite[Start] = c.start
          val run: Start => IOLite[B] = (s: c.Start) =>
            new IOLite.Compute[B] {
              type Start = A
              val start: () => IOLite[A] = () => c.run(s)
              val run: A => IOLite[B] = f
            }
        }
      case _ =>
        new IOLite.Compute[B] {
          type Start = A
          val start: () => IOLite[A] = () => self
          val run: A => IOLite[B] = f
        }
    }
  }

}

object IOLite {

  def pure[A](a: A): IOLite[A] = {
    Pure(a)
  }

  def primitive[A](a: => A): IOLite[A] = {
    new Primitive(a _)
  }

  private final case class Pure[A](unsafePerformIO: A) extends IOLite[A]

  private final class Primitive[A](f: () => A) extends IOLite[A] {
    def unsafePerformIO: A = f()
  }

  private sealed abstract class Compute[A] extends IOLite[A] {
    type Start

    val start: () => IOLite[Start]

    val run: Start => IOLite[A]

    def unsafePerformIO: A = {
      type IOLiteAny = IOLite[Any]
      type IOLiteAnyFunc = Any => IOLite[Any]

      @tailrec
      def loop(curr: IOLiteAny, funcs: List[IOLiteAnyFunc]): Any = {
        curr match {

          case c: Compute[_] =>
            c.start() match {
              case cc: Compute[_] =>
                loop(
                  cc.start().asInstanceOf[IOLiteAny],
                  cc.run.asInstanceOf[IOLiteAnyFunc] :: c.run.asInstanceOf[IOLiteAnyFunc] :: funcs
                )

              case xx =>
                loop(c.run(xx.unsafePerformIO).asInstanceOf[IOLiteAny], funcs)
            }

          case x =>
            funcs match {
              case headFunc :: tail => loop(headFunc(x.unsafePerformIO), tail)
              case Nil => x.unsafePerformIO
            }
        }
      }

      loop(this.asInstanceOf[IOLiteAny], Nil).asInstanceOf[A]
    }
  }

}

object X {
  def main(args: Array[String]): Unit = {
    println(IOLite.pure[Int](4).unsafePerformIO)
    println(IOLite.primitive[Int](4).unsafePerformIO)

    val total: IOLite[Int] =
      IOLite.primitive(slowGet(10))
        .flatMap(i => slowGetM(i + 30))
        .map(i => i + slowGet(20))
    println(total.unsafePerformIO)
  }

  def slowGet(i: Int): Int = {
    Thread.sleep(300)
    i
  }

  def slowGetM(i: Int): IOLite[Int] = {
    Thread.sleep(300)
    IOLite.primitive(i)
  }
}
