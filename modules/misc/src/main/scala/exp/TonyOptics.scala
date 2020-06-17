package exp

import cats._
import cats.data._
import cats.implicits._


// https://blog.tmorris.net/posts/classy-optics-error-handling-scala/

trait Optic[~>[_, _], F[_], A, B] {
  def run: (B ~> F[B]) => A ~> F[A]
}

object Optic {
  def get[A, B](optic: Optic[Function1, Const[B, *], A, B]): A => B =
    a => optic.run(b => Const(b))(a).getConst

  def set[A, B](optic: Optic[Function1, Id, A, B]): A => B => A =
    a => b => optic.run(_ => b)(a)
}

////

final case class Person(age: Int, name: String)
object Person {

  def age[F[_]: Functor]: Optic[Function1, F, Person, Int] =
    new Optic[Function1, F, Person, Int] {
      def run =
        k =>
          person => {
            k(person.age)
              .map(a => Person(a, person.name))
          }
    }

  def name[F[_]: Functor]: Optic[Function1, F, Person, String] =
    new Optic[Function1, F, Person, String] {
      def run =
        k =>
          person => {
            k(person.name)
              .map(n => Person(person.age, n))
          }
    }

  def main(args: Array[String]): Unit = {
    import Optic._

    val p0 = Person(45, "Fred")
    println(get[Person, Int](age)(p0)) // 45
    println(get[Person, String](name)(p0)) // Fred

    val p1 = set[Person, Int](age)(p0)(46)
    println(p1) // Person(46,Fred)

    val p2 = set[Person, String](name)(p1)("Mary")
    println(p2) // Person(46,Mary)
  }
}
