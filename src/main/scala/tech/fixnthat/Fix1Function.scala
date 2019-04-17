package tech.fixnthat

object Fix1Function {

  def factorial(f: Int => Int): Int => Int = {
    case 1 => 1
    case n => n * f(n - 1)
  }

  // script: ghci

  //Prelude> fac n = if (n < 3) then n else n * fac (n - 1)
  //Prelude> :t fac
  //facexp :: (Ord t, Num t) => t -> t

  //Prelude> fac f n = if (n < 3) then n else n * f (n - 1)
  //Prelude> :t fac
  //fac :: (Ord t, Num t) => (t -> t) -> t -> t

  //Prelude> rec g = g (rec g)
  //Prelude> :t rec
  //rec :: (t -> t) -> t

  //Prelude> fixfac = rec fac
  //Prelude> :t fixfac
  //fixfac :: (Ord t, Num t) => t -> t

  //Prelude> :t fixfac 5
  //fixfac 5 :: (Ord t, Num t) => t

  // script: start with
  //  rec :: (t -> t) -> t
  //  rec f = f (rec f)

  // script: convert naively to scala
  //  def rec[A, B]: (T => T) => T =
  //    f => f(rec(f))

  // script: run it - stack overflow because scala is strict

  // script: note that T is actually A => A, eg Int => Int, so change each T into (A => A), ie
  //  def rec[A, B]: ((A => A) => (A => A)) => (A => A) =
  //    f => f(rec(f))

  // script: run it - stack overflow again

  // script: note that returning (A => A) is same as returning A => A, so need to introduce an a parameter
  // so we have f => a => ..., so need to apply the a at the end:
  //  def rec[A, B]: ((A => A) => (A => A)) => A => A =
  //    f => a => f(rec(f))(a)

  def rec[A]: ((A => A) => A => A) => A => A =
    f => a => f(rec(f))(a)

  // script: run now works

  // script: rename rec to fix

  def main(args: Array[String]): Unit = {
    val fixed: Int => Int = rec(factorial)
    println(fixed(6))

    // script: origin of name `fix point`
    //    x = cos(x)

    //    var x = 123.4 // don't @ me
    //    for (_ <- 0 to 100) {
    //      println(x)
    //      x = Math.cos(x)
    //    }
    //    println(x)

    //    if (true) {
    //      // solve x*x - 2x - 1 = 0
    //      // x*x - 2x - 1 = 0
    //      // x*x = 2x + 1
    //      // x = 2 + (1/x)
    //      var x = 123.4 // don't @ me
    //      for (_ <- 0 to 10) {
    //        println(x)
    //        x = 2 + (1 / x)
    //      }
    //      println(x)
    //    }

    if (true) {
      // solve x*x + 2x + 1 = 0
      // x*x = -2x - 1
      // x = -2 - (1/x)
      var x = 10.0 // don't @ me
      for (_ <- 0 to 10) {
        x = -2.0 - (1.0 / x)
      }
      println(x)
    }

  }
}
