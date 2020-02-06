package redbook

import cats.Functor
import cats.implicits._

object ch04 {
  def main(args: Array[String]): Unit = {
    def sToI(s: String): Int = Integer.parseInt(s)

    val lifted: Option[String] => Option[Int] = Functor[Option].lift(sToI)
    val voided: Option[Unit] = Functor[Option].void(Option(4))

    // 4.05
    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
      as.foldRight[Option[List[B]]](Option(List())) {
        (a, acc) =>
          acc.fold(None: Option[List[B]]) {
            lb =>
              f(a).fold(None: Option[List[B]]) {
                bb => Option(bb :: lb)
              }
          }
      }

    // 4.04
    def sequence[A](as: List[Option[A]]): Option[List[A]] =
      as.foldRight[Option[List[A]]](Option(List())) {
        (oa, acc) =>
          oa.fold(None: Option[List[A]])(a => acc.map(a :: _))
      }

    // 4.03
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      (a, b) match {
        case (Some(aa), Some(bb)) => Some(f(aa, bb))
        case (_, _) => None
      }
  }
}
