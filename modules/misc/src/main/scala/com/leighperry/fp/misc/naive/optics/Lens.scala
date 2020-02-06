package naive.optics

// Based on Monocle
trait Lens[S, A] {
  self =>

  def get(s: S): A
  def set(a: A): S => S

  def modify(f: A => A): S => S =
    s => set(f(get(s)))(s)

  // S contains an A, A contains a B
  def composeLens[B](other: Lens[A, B]): Lens[S, B] =
    new Lens[S, B] {
      override def get(s: S): B = other.get(self.get(s))
      override def set(b: B): S => S =
        s => {
          val a: A = self.get(s)
          val newA: A = other.set(b)(a)
          self.set(newA)(s)
        }
    }

}

object Lens {
  def lens[S, A](getter: S => A, setter: A => S => S): Lens[S, A] =
    new Lens[S, A] {
      override def get(s: S): A = getter(s)
      override def set(a: A): S => S = setter(a)
    }
}

////

object LensTest {

  final case class Suburb(name: String, postcode: Int)
  object Suburb {
    val lensName = Lens.lens[Suburb, String](_.name, a => s => s.copy(name = a))
    val lensPostcode = Lens.lens[Suburb, Int](_.postcode, a => s => s.copy(postcode = a))
  }

  final case class Address(number: Int, street: String, suburb: Suburb)
  object Address {
    val lensNumber = Lens.lens[Address, Int](_.number, a => s => s.copy(number = a))
    val lensStreet = Lens.lens[Address, String](_.street, a => s => s.copy(street = a))
    val lensSuburb = Lens.lens[Address, Suburb](_.suburb, a => s => s.copy(suburb = a))
  }

  final case class Person(name: String, age: Int, address: Address)
  object Person {
    val lensName = Lens.lens[Person, String](_.name, a => s => s.copy(name = a))
    val lensAge = Lens.lens[Person, Int](_.age, a => s => s.copy(age = a))
    val lensAddress = Lens.lens[Person, Address](_.address, a => s => s.copy(address = a))
  }

  def main(args: Array[String]): Unit = {
    val lPersonPostcode: Lens[Person, Int] =
      Person.lensAddress.composeLens(Address.lensSuburb.composeLens(Suburb.lensPostcode))

    val person = Person("Albert", 34, Address(12, "Einstein St", Suburb("CBD", 2000)))
    val personUpdated = lPersonPostcode.modify(_ + 1000)(person)

    println(personUpdated) // move to Victoria
  }
}
