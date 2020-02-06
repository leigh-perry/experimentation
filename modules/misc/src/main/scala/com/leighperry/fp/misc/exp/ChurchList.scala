package exp

object ChurchList {
  //  newtype ListL a =
  //    ListL {
  //      build :: forall b. (b -> a -> b) -> b -> b
  //    }

  //  mapL :: (a -> a') -> ListL a -> ListL a'
  //  mapL f l = ListL (\f' b' -> build l (\b a -> f' b (f a)) b')

  final case class ListL[A, B](build: (B => A => B) => B => B)

  def mapL[A, B, AA]: (A => AA) => ListL[A, B] => ListL[AA, B] =
    f => l => {
      val buildAA: (B => AA => B) => B => B =
        ff => bb => {
          val tail: B => A => B =
            b => a => {
              val aaToB: AA => B = ff(b)
              aaToB(f(a))
            }
          l.build(tail)(bb)
        }

      ListL[AA, B](buildAA)
    }
}
