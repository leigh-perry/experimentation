package redbook

object ch06 {

  def main(args: Array[String]): Unit = {
    println(ints(1)(SimpleRNG(0)))
    println(ints(4)(SimpleRNG(0)))
    println(ints(14)(SimpleRNG(0)))
    println(nonNegativeEven(SimpleRNG(0)))
  }

//  // 6.07
//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//    fs.foldRight()
//  }

  // 6.06
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r2) = ra(rng)
      val (b, r3) = rb(r2)
      (f(a, b), r3)
    }

  // 6.05
  def double: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  type Rand[+A] = RNG => (A, RNG)
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.04
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case 0 => (Nil, rng)
      case _ =>
        val (i, r2) = rng.nextInt
        val (l, r3) = ints(count - 1)(r2)
        (i :: l, r3)
    }
  }

  // 6.01
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng2) = rng.nextInt
    (if (v == Int.MinValue) 0 else math.abs(v), rng2)
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
}