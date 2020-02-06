package naive.optics

trait Optional[S, A] {
  self =>

  def set(a: A): S => S
  def getOption(s: S): Option[A]

  def modify(f: A => A): S => S =
    s =>
      self
        .getOption(s)
        .fold(s) {
          a =>
            self.set(f(a))(s)
        }

  def composeOptional[C](other: Optional[A, C]): Optional[S, C] =
    new Optional[S, C] {
      override def set(c: C): S => S =
        self.modify(other.set(c)) // can't implement purely chaining set() ... such is optionality

      override def getOption(s: S): Option[C] =
        self
          .getOption(s)
          .fold(Option.empty[C]) {
            a =>
              other.getOption(a)
          }
    }
}

object Optional {
  def optional[S, A](setter: A => S => S, getter: S => Option[A]): Optional[S, A] =
    new Optional[S, A] {
      override def set(a: A): S => S =
        setter(a)

      override def getOption(s: S): Option[A] =
        getter(s)
    }
}

////

object OptionalTest {
  def main(args: Array[String]): Unit = {
    def optionalHead[A]: Optional[List[A], A] =
      Optional.optional(
        a =>
          list =>
            list match {
              case Nil => a :: Nil
              case _ :: tail => a :: tail
            },
        list =>
          list match {
            case Nil => Option.empty[A]
            case head :: _ => Some(head)
          }
      )

    println(optionalHead.getOption(List(1, 2, 3)))
    println(optionalHead.getOption(List.empty))

    println(optionalHead.set(7)(List(1, 2, 3)))
    println(optionalHead.set(7)(List.empty))
  }
}
