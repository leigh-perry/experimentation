package tech.monoidnthat

import cats.instances.double._
import cats.instances.int._
import cats.instances.set._
import cats.instances.string._
import cats.instances.unit._
import cats.kernel.Monoid

object Monoid1Intro {

  // trait Semigroup[A] {
  //   def combine(x: A, y: A): A
  // }

  // trait Monoid[A] extends Semigroup[A] {
  //   def empty: A
  // }

  val mInt = Monoid[Int]
  val mString = Monoid[String]
  val mSet = Monoid[Set[Int]]
  //val mBoolean = Monoid[Boolean]
  val mUnit = Monoid[Unit]

  val mDouble = Monoid[Double]
}
