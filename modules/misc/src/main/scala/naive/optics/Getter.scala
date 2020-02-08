package naive.optics

abstract class Getter[S, A] {
  self =>

  def get(s: S): A
}
