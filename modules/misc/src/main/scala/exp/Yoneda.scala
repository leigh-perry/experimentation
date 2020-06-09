package exp

import cats.Functor
import cats.instances.list._

// See
//    https://medium.com/@olxc/yoneda-and-coyoneda-trick-f5a0321aeba4
//    http://blog.higher-order.com/blog/2013/11/01/free-and-yoneda/

trait Yoneda[F[_], A] {
  self =>

  def mapF[B](f: A => B): F[B]

  def map[B](f: A => B): Yoneda[F, B] =
    new Yoneda[F, B] {
      override def mapF[C](g: B => C): F[C] =
        self.mapF(g compose f)
    }

  def run: F[A] =
    mapF(identity)
}

object Yoneda {
  def toYoneda[F[_]: Functor, A](fa: F[A]): Yoneda[F, A] =
    new Yoneda[F, A] {
      override def mapF[B](f: A => B): F[B] =
        Functor[F].map(fa)(f)
    }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    val y = Yoneda.toYoneda(list)
    val y2 = y.map(_ + 1).map(_ * 2).map(_ + 10)
    val list2 = y2.run
    println(list2)
  }

}

sealed trait Coyoneda[F[_], A] {
  type UnderlyingType
  val underlyingValue: F[UnderlyingType]
  val transformation: UnderlyingType => A

  def run(f: Functor[F]): F[A] =
    f.map(underlyingValue)(transformation)

  def map[B](f: A => B): Coyoneda[F, B] =
    Coyoneda.toCoyoneda(underlyingValue)(f compose transformation)
}

object Coyoneda {
  def toCoyoneda[F[_], A, B](fa: F[A])(f: A => B): Coyoneda[F, B] =
    new Coyoneda[F, B] {
      override type UnderlyingType = A // save original type
      override val transformation: A => B = f
      override val underlyingValue: F[A] = fa
    }

  def main(args: Array[String]): Unit = {
    final case class Person[A](a: A) // not a functor

    val personCoyo0: Coyoneda[Person, Int] = toCoyoneda(Person(42))(identity)
    val personCoyo1: Coyoneda[Person, Int] = personCoyo0.map(_ + 1).map(_ + 2).map(_ + 3)

    // nothing is executed yet
    // define functor for Person
    val personFunctor =
      new Functor[Person] {
        override def map[A, B](fa: Person[A])(f: A => B): Person[B] =
          Person(f(fa.a))
      }

    // and then pass it to Coyoneda
    val person: Person[Int] = personCoyo1.run(personFunctor)
    println(person) // should yield Person(48)

    // NB for Free[Coyoneda[...]], no functor is required since Free is interpreted via ~> to IO instead
  }
}
