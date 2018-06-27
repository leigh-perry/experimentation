package exp

import cats.Apply
import cats.data.Kleisli
import cats.implicits._

object TestKleisli {
  def main(args: Array[String]): Unit = {
    //testSimple()
    testConfig()
  }

  private def testConfig(): Unit = {
    final case class Name(first: String, last: String)

    final case class Age(age: Int)

    final case class Person(name: Name, age: Age)

    final case class Config(name: String, age: Int)

    def readName: Config => Option[Name] = c => {
      val parts = c.name.split(" ")
      if (parts.length > 1) Option(Name(parts(0), parts.drop(1).mkString(" "))) else None
    }

    val readNameK: Kleisli[Option, Config, Name] = Kleisli(readName)

    def readAge: Config => Option[Age] = c => if (c.age >= 1 && c.age <= 150) Option(Age(c.age)) else None

    val readAgeK = Kleisli(readAge)

    val cfg = Config("Leigh Perry", 49)

    val readPersonK: Kleisli[Option, Config, Person] =
      for {
        n <- readNameK
        a <- readAgeK
      } yield Person(n, a)
    println(readPersonK(cfg))

    val readPersonKA: Kleisli[Option, Config, Person] = (readNameK, readAgeK).mapN(Person)
    println(readPersonKA(cfg))

    type KOptionConfig[A] = Kleisli[Option, Config, A]
    val readNameKOC: KOptionConfig[Name] = readNameK
    val readAgeKOC: KOptionConfig[Age] = readAgeK
    val readPersonKA2 = Apply[KOptionConfig].map2(readNameKOC, readAgeKOC)(Person)
    println(readPersonKA2(cfg))
  }

  private def testSimple() = {
    def intString: Int => Option[String] = _.toString.some

    def stringLength: String => Option[Int] = _.length.some

    def intStringK: Kleisli[Option, Int, String] = Kleisli(intString)

    def stringLengthK: Kleisli[Option, String, Int] = Kleisli(stringLength)

    def composed = intStringK andThen stringLengthK

    println(composed(1234))

    val l0: Option[Int] =
      for {
        s <- intString(1234)
        l <- stringLength(s)
      } yield l
    println(l0)

    val l1: Option[Int] =
      for {
        s <- intStringK(1234)
        l <- stringLengthK(s)
      } yield l
    println(l1)
  }
}
