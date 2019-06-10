package tech.gentypes

import cats.{Eq, Show}

import scala.collection.immutable.List

final case class Coll[+A] private(list: List[A], lineage: List[(String, Coll[Any])] = Nil) {
  def map[B](f: A => B): Coll[B] =
    Coll(list.map(f), ("map", this) :: lineage)

  def flatMap[B](f: A => Coll[B]): Coll[B] =
    Coll(list.flatMap(f andThen (_.list)), ("flatMap", this) :: lineage)

  def filter(c: A => Boolean): Coll[A] =
    Coll(list.filter(c), ("filter", this) :: lineage)

  def distinct =
    Coll(list.distinct, ("distinct", this) :: lineage)

  def reverse =
    Coll(list.reverse, ("reverse", this) :: lineage)

  def length =
    list.length
}

object Coll {

  implicit def eq[A]: Eq[Coll[A]] =
    new Eq[Coll[A]] {
      override def eqv(x: Coll[A], y: Coll[A]): Boolean =
        x.list == y.list
    }

  implicit def show[A]: Show[Coll[A]] =
    new Show[Coll[A]] {
      override def show(t: Coll[A]): String =
        "\n" +
          t.lineage.reverse.zipWithIndex
            .foldRight(dumpList(t.list)) {
              case (((op: String, a: Coll[Any]), i: Int), b: String) =>
                s"""
                   |$b
                   |${"  " * (t.lineage.length - i)}$op: ${dumpList(a.list)}
                   |""".stripMargin.trim
            }
    }

  def dumpList[A](list: List[Any]): String =
    if (list.length == 0) {
      "[]"
    } else {
      list
        .map(_.toString)
        .mkString(s"${list.head.getClass.getSimpleName}: [", ", ", "]")
    }
}






































object Temp {

  def isSorted[A](l: List[A]): Boolean =
    true
}