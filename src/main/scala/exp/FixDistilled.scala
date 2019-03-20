package exp

import java.util.concurrent.atomic.AtomicInteger

object DirectRecursion {
  def factorial: Int => Int =
    n =>
      n match {
        case 1 => 1
        case _ => n * factorial(n - 1)
      }

  def main(args: Array[String]): Unit =
    println(factorial(3))
}

////

//object DirectRecursionX {
//  def factorial(step: Int => Int): Int => Int =
//    n =>
//      n match {
//        case 1 => 1
//        case _ => n * step(n - 1)
//      }
//
//  def main(args: Array[String]): Unit = {
//    // println(factorial(factorial)(3))
//    ()
//  }
//}

////

object FixDistilled {

  def factorial(step: Int => Int): Int => Int =
    n =>
      n match {
        case 1 => 1
        case _ => n * step(n - 1)
      }

  def fix[T, R](f: (T => R) => (T => R)): T => R =
    new Function[T, R] {
      override def apply(t: T): R =
        f(this)(t)
    }

  def main(args: Array[String]): Unit = {
    // fix(factorial) puts a wrapper around factorial via new Function1[] { f(this)(t) }
    // then calls the function, passing the wrapper for the recursion function too

    // there are no direct recursive calls anywhere -- corecursion

    // Y = λf.(λx.f (x x)) (λx.f (x x))

    val wrapped = fix(factorial)
    println(wrapped(6))
  }
}

////

object FixDistilledVerbose {
  private val spaces = new AtomicInteger(0) // don't @ me
  private def indent = spaces.addAndGet(2)
  private def outdent = spaces.addAndGet(-2)
  private def output(s: String) =
    println(" " * spaces.get + s)

  def fix[T, R](f: (T => R) => (T => R)): T => R = {
    output("fix creating wrapper function")
    new Function[T, R] {
      override def apply(t: T): R = {
        indent
        output(s"in fix wrapper $t")
        val fBound: T => R = f(this)
        output(s"invoking stepper with $t")

        indent
        val r: R = fBound(t)
        outdent

        output(s"fix wrapper result $r")
        outdent
        r
      }
    }
  }

  // factorial adds the business logic (multiplication with predecessor)
  // step is some undefined way of invoking the next step in the chain
  // in practice step is the Function1 instance created by fix(factorial)
  def factorial(step: Int => Int): Int => Int = {
    output("creating stepper")
    n => {
      output(s"stepper ($n)")
      val r =
        n match {
          case 1 => 1
          case _ => {
            val next = n - 1
            output(s"stepping $next")
            val result = n * step(next)
            output(s"back from stepping $next")
            result
          }
        }
      output(s"factorial finished invoke $n => $r")
      r
    }
  }

  def fib(step: Int => Int): Int => Int =
    n =>
      n match {
        case 0 | 1 => 1
        case n => step(n - 1) + step(n - 2)
      }

  def main(args: Array[String]): Unit = {
    // fix(factorial) puts a wrapper around factorial via new Function1[] { f(this)(t) }
    // then calls the function, passing the wrapper for the recursion function too

    // NOTE: fix never calls itself - no recursion
    // NOTE: factorial never calls itself - no recursion
    // ... there are no recursive calls anywhere

    // https://www.youtube.com/watch?v=9T8A89jgeTI
    // Y = λf.(λx.f (x x)) (λx.f (x x))
    // def fix[T, R](f: (T => R) => (T => R)): T => R = {
    // def fix[T, R]: ((T => R) => (T => R)) => T => R = {

    // NOTE: we never invoke factorial directly, it is called via the thunk

    // NOTE: the wrapper function becomes a thunk for taking the 'callback'
    // and passing it down into factorial() again ... couldn't do that directly

    val fixed = fix(factorial)
    output(s"result ${fixed(3)}")

    //output((0 to 10).map(fix(fib)(_)))
  }

}
