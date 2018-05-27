package demo

object TaskCallback {

  // function with which we can register a callback
  case class Async[A](task: (A => Unit) => Unit) {

    def unsafePerform: A = {
      var result: Option[A] = None

      val cb: A => Unit = a => result = Some(a)
      task(cb)

      result.get
    }
  }

  def main(args: Array[String]): Unit = {

    if (true) {
      def task(cb: Int => Unit): Unit = {
        // do something and call back with result
        println("before")
        cb(1234)

        // do more if needed
        println("after")
      }

      val async = Async(task)
      val value = async.unsafePerform
      println(value)

      assert(value == 1234)
    }

    if (true) {
      val task: (Int => Unit) => Unit = {
        cb =>
          // do something and call back with result
          println("before")
          cb(1234)

          // do more if needed
          println("after")
      }

      val async = Async(task)
      val value = async.unsafePerform
      println(value)

      assert(value == 1234)
    }

    ()
  }

}
