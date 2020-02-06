package tech.gentypes

import cats.{Eq, Show}

import scala.collection.immutable.List

final case class Coll[+A] private(list: List[A], lineage: List[(String, Coll[Any])]) {
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
  def of[A](l: List[A]) =
    Coll(l, Nil)

  implicit def eq[A]: Eq[Coll[A]] =
    new Eq[Coll[A]] {
      override def eqv(x: Coll[A], y: Coll[A]): Boolean =
        x.list == y.list  // exclude lineage
    }

  implicit def show[A]: Show[Coll[A]] =
    new Show[Coll[A]] {
      override def show(c: Coll[A]): String = {
        val lineage = c.lineage.reverse
        val ops = "init" :: lineage.map(_._1)
        val lists = lineage.map(_._2.list) :+ c.list
        ops.zip(lists)
          .zipWithIndex
          .foldLeft("") {
            case (b: String, ((op: String, l: List[Any]), i: Int)) =>
              s"""
                 |$b
                 |${"  " * i}$op: ${dumpList(l)}
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
}

























object Temp {

  def isSorted[A](l: List[A]): Boolean =
    true
}