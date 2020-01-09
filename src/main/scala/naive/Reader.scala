package naive

case class Reader[R, A](provide: R => A) {
  self =>

  def map[B](f: A => B): Reader[R, B] =
    flatMap[B] {
      a =>
        Reader.point(f(a))
    }

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
    Reader[R, B] {
      r =>
        val a: A = self.provide(r)
        println(s"intermed $r => $a")
        val b: B = f(a).provide(r)
        println(s"popping $a => $b")
        b
    }
}

object Reader {
  def point[R, A](a: => A): Reader[R, A] =
    Reader(_ => a)

  def environment[R]: Reader[R, R] =
    Reader(identity)

  def access[R, A](f: R => A): Reader[R, A] =
    environment[R].map(f) // same as Reader(f)
}

object TestReader {
  final case class Config(serverName: String, port: Int)

  def main(args: Array[String]): Unit = {
    val r: Reader[Int, String] =
      for {
        a <- Reader[Int, Long](_ * 1000L)
        b <- Reader[Int, Char](_.asInstanceOf[Char])
        c <- Reader[Int, String](_ + s" / $a $b")
      } yield c
    println(r.provide(88))
    println("----------------")

    val serverName: Reader[Config, String] =
      Reader.access[Config, String](_.serverName)

    println(serverName.provide(Config("localhost", 43)))
  }
}
