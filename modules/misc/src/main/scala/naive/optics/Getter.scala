package naive.optics

import naive.optics.LensTest.{ Address, Person, Suburb }

abstract class Getter[S, A] {
  self =>

  def get(s: S): A

  def composeGetter[B](other: Getter[A, B]): Getter[S, B] =
    new Getter[S, B] {
      override def get(s: S): B =
        other.get(self.get(s))
    }

}

object Getter {
  def getter[S, A](getr: S => A): Getter[S, A] =
    new Getter[S, A] {
      override def get(s: S): A = getr(s)
    }
}

////

object GetterTest {

  def main(args: Array[String]): Unit = {
    val ga = Getter.getter[Person, Address](_.address)
    val rodney = Person("Rodney", 134, Address(12, "Kent", Suburb("CBD", 2000)))

    println(ga.get(rodney))

    val gas = ga.composeGetter(Getter.getter[Address, Suburb](_.suburb))
    println(gas.get(rodney))

    val gasp = gas.composeGetter(Getter.getter[Suburb, Int](_.postcode))
    println(gasp.get(rodney))
  }
}
