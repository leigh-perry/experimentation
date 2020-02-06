package tech.fixnthat

object Fix1Function {

  def main(args: Array[String]): Unit = {

    // >>> factorial in haskell, refactor to rec

    /*
    Prelude> fac n = if (n < 3) then n else n * fac (n - 1)
    Prelude> :t fac
    fac :: (Ord t, Num t) => t -> t

    Prelude> fac f n = if (n < 3) then n else n * f (n - 1)
    Prelude> :t fac
    fac :: (Ord t, Num t) => (t -> t) -> t -> t

     Prelude> fac fac 6

    <interactive>:6:1: error:
        • Non type-variable argument in the constraint: Ord (t -> t)
          (Use FlexibleContexts to permit this)
        • When checking the inferred type
            it :: forall t.
                  (Ord t, Ord (t -> t), Num t, Num (t -> t)) =>
                  t -> t


    Prelude> rec g = g (rec g)
    Prelude> :t rec
    rec :: (t -> t) -> t

    Prelude> fixfac = rec fac
    Prelude> :t fixfac
    fixfac :: (Ord t, Num t) => t -> t

    Prelude> :t fixfac 5
    fixfac 5 :: (Ord t, Num t) => t
    */

    // >>> port to scala
    // start with
    //   rec :: (t -> t) -> t
    //   rec f = f(rec f)

    def rec[A]: ((A => A) => A => A) => A => A =
      f => a => f(rec(f))(a)

    /*
    >>> convert naively to scala
     def rec[A, B]: (T => T) => T =
       f => f(rec(f))

    >>> run it - stack overflow because scala is strict

    >>> note that T is actually A => A, eg Int => Int, so change each T into (A => A), ie
     def rec[A, B]: ((A => A) => (A => A)) => (A => A) =
       f => f(rec(f))

    >>> run it - stack overflow again

    >>> note that returning (A => A) is same as returning A => A, so need to introduce an a parameter
    so we have f => a => ..., so need to apply the a at the end:
     def rec[A, B]: ((A => A) => (A => A)) => A => A =
       f => a => f(rec(f))(a)

    >>> run now works
    */


    def factorial: (Int => Int) => Int => Int =
      f => n => if (n == 1) 1 else n * f(n - 1)

    val fixed: Int => Int = rec(factorial)
    println(fixed(6))

    // >>> rename rec to fix

    if (false) {
      // >>> origin of name `fix point`
      // A fixed point of a function is a value that, when applied as
      // the input of the function, returns
      // the same value as its output
      //
      //    x = cos(x)

      var x = 123.4 // don't @ me
      for (_ <- 0 to 50) {
        println(x)
        x = Math.cos(x)
      }
      println(x)
    }

    if (false) {
      // related to numerical methods
      //
      // solve x*x + 2x + 1 = 0
      // x*x = -2x - 1
      // x = -2 - (1/x)
      var x = 10.0
      for (_ <- 0 to 10) {
        x = -2.0 - (1.0 / x)
        println(x)
        x
      }
      println(x)
    }

  }
}
