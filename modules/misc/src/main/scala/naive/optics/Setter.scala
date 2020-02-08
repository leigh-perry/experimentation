package naive.optics

abstract class Setter[S, A] {
  self =>

  def modify(f: A => A): S => S

  def set(a: A): S => S

  def composeSetter[C](other: Setter[A, C]): Setter[S, C] =
    ???
}
