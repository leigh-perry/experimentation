package naive.optics

trait Iso[S, A] {
  self =>

  def get(s: S): A
  def reverseGet(a: A): S

  def composeIso[C](other: Iso[A, C]): Iso[S, C] =
    new Iso[S, C] {
      override def get(s: S): C =
        other.get(self.get(s))

      override def reverseGet(c: C): S =
        self.reverseGet(other.reverseGet(c))
    }

  /** Source becomes the target and the target becomes the source */
  def reverse: Iso[A, S] =
    new Iso[A, S] {
      override def get(a: A): S =
        self.reverseGet(a)

      override def reverseGet(s: S): A =
        self.get(s)
    }
}

object Iso {
  def iso[S, A](getter: S => A, setter: A => S): Iso[S, A] =
    new Iso[S, A] {
      override def get(s: S): A =
        getter(s)

      override def reverseGet(a: A): S =
        setter(a)
    }
}

////

object IsoTest {
  val isoMm2Cm = Iso.iso[Double, Double](_ / 10, _ * 10)
  val isoCm2M = Iso.iso[Double, Double](_ / 100, _ * 100)
  val isoMm2M = isoMm2Cm.composeIso(isoCm2M)

  def main(args: Array[String]): Unit = {
    println(s"1200.0 mm = ${isoMm2M.get(1200.0)} m")
    println(s"1.2 m = ${isoMm2M.reverseGet(1.2)} mm")
  }
}
