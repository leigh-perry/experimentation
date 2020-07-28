package tech.fixnthat

import cats.data.{Nested, NonEmptyList}
import exp.Rendering
import tech.fixnthat.Fix2Type.Fix

object Fix4ComposeListNel {

  // >>> Relative complexity of List and NEL - equivalent
  type XList[A] = Option[NonEmptyList[A]]
  type XNonEmptyList[A] = (A, List[A])

  //  type NonEmpty a = Fix (Compose ((,) a) Maybe)
  //  type List a =     Fix (Compose Maybe ((,) a))
  type CNonEmpty[A] = Fix[Nested[(A, ?), Option, ?]] // (A, Option[?])
  type CList[A]     = Fix[Nested[Option, (A, ?), ?]] // Option[(A, ?)]

  /*
  ne1, ne2 :: NonEmpty Int
  ne1 = Fix (Compose (1, Nothing))
  ne2 = Fix (Compose (1, Just (Fix (Compose (2, Nothing)))))

  l0, l1, l2 :: List Int
  l0 = Fix (Compose Nothing)
  l1 = Fix (Compose (Just (1, Fix (Compose Nothing))))
  l2 = Fix (Compose (Just (1, Fix (Compose (Just (2, Fix (Compose Nothing)))))))
  */

  def main(args: Array[String]): Unit = {

    // >>> NEL example
    if (false) {
      // type CNel[A] = Fix[Nested[(A, ?), Option, ?]]

      val nel1 = nelCons((1, Some(nelCons((-1, None)))))
      //Rendering.of(nel1, "4-nel1")

      val nel12 = nelCons((2, Some(nel1)))
      //Rendering.of(nel12, "4-nel12")

      println(nel12)
    }

    // >>> List example
    if (false) {
      // type CList[A] = Fix[Nested[Option, (A, ?), ?]]

      val listNil: Fix[Nested[Option, (Int, ?), ?]] = listCons(None)
      //Rendering.of(listNil, "4-listNil")

      val list1 = listCons(Some((1, listNil)))
      //Rendering.of(list1, "4-list1")

      val list12 = listCons(Some((2, list1)))
      //Rendering.of(list12, "4-list12")

      println(list12)
    }
  }

  ////

  def nelCons[A](entry: (A, Option[Fix[Nested[(A, ?), Option, ?]]])) =
    nelFix(Nested[(A, ?), Option, Fix[Nested[(A, ?), Option, ?]]](entry))

  def nelFix[A](tail: Nested[(A, ?), Option, Fix[Nested[(A, ?), Option, ?]]]) =
    Fix[Nested[(A, ?), Option, ?]](tail)

  ////

  def listCons[A](opt: Option[(A, Fix[Nested[Option, (A, ?), ?]])]): Fix[Nested[Option, (A, ?), ?]] =
    listFix(Nested[Option, (A, ?), Fix[Nested[Option, (A, ?), ?]]](opt))

  def listFix[A](tail: Nested[Option, (A, ?), Fix[Nested[Option, (A, ?), ?]]]) =
    Fix[Nested[Option, (A, ?), ?]](tail)

}
