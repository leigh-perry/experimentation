package naive.optics

import cats.Monoid

abstract class Fold[S, A] {
  self =>

  // underlying representation of [[Fold]], all [[Fold]] methods are defined in terms of foldMap
  def foldMap[M: Monoid](f: A => M)(s: S): M
}
