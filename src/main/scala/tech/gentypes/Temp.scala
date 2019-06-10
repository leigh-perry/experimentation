package tech.gentypes

import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.arbitrary

class Temp {
  val af: Gen[Boolean => Int] = arbitrary[Boolean => Int]

  def afa[A](implicit cogenA: Cogen[A]): Gen[A => Int] = arbitrary[A => Int]
}
