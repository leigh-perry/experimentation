package naive.optics

import naive.optics.LensTest.{ Address, Person, Suburb }

abstract class Setter[S, A] {
  self =>

  def modify(f: A => A): S => S

  def set(a: A): S => S =
    modify(_ => a)

  def composeSetter[C](other: Setter[A, C]): Setter[S, C] =
    new Setter[S, C] {
      override def modify(f: C => C): S => S =
        self.modify(other.modify(f))
    }
}

object Setter {
  def setter[S, A](setr: (A => A) => S => S): Setter[S, A] =
    new Setter[S, A] {
      override def modify(f: A => A): S => S =
        setr(f)
    }
}

////

object SetterTest {

  def main(args: Array[String]): Unit = {
    val ga = Setter.setter[Person, Address](f => s => s.copy(address = f(s.address)))
    val rodney = Person("Rodney", 134, Address(12, "Kent", Suburb("CBD", 2000)))

    println(ga.set(Address(22, "Lonsdale", Suburb("CBD", 3000)))(rodney))

    val gas = ga.composeSetter(Setter.setter[Address, Suburb](f => s => s.copy(suburb = f(s.suburb))))
    println(gas.set(Suburb("Proserpine", 4800))(rodney))

    val gasp = gas.composeSetter(Setter.setter[Suburb, Int](f => s => s.copy(postcode = f(s.postcode))))
    println(gasp.set(2001)(rodney))
  }
}
