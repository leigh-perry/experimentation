package tech.v2

object FunctorVariance {

  final case class Inner(i: Int)
  final case class Outer(inner: Inner)

  ////

  trait Decoder[+A] {
    def fromString(s: String): A
  }

  trait CovariantFunctor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  val aDecoderCovariantFunctor: CovariantFunctor[Decoder] =
    new CovariantFunctor[Decoder] {
      override def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] =
        new Decoder[B] {
          override def fromString(s: String): B = f(fa.fromString(s))
        }
    }

  val aDecoderForOuter =
    new Decoder[Outer] {
      override def fromString(s: String): Outer = Outer(Inner(s.toInt))
    }

  val aDecoderForInner: Decoder[Inner] = aDecoderCovariantFunctor.map(aDecoderForOuter)(_.inner)

  ////

  trait Encoder[-A] {
    def toString(a: A): String
  }

  trait ContravariantFunctor[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  val aEncoderContravariantFunctor: ContravariantFunctor[Encoder] =
    new ContravariantFunctor[Encoder] {
      override def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] =
        new Encoder[B]() {
          override def toString(b: B): String = fa.toString(f(b))
        }
    }

  val aEncoderForInner =
    new Encoder[Inner] {
      override def toString(a: Inner): String = s"Inner(${a.i})"
    }

  val aEncoderForOuter: Encoder[Outer] =
    aEncoderContravariantFunctor.contramap(aEncoderForInner)(_.inner)

  ////

  trait InvariantFunctor[F[_]] {
    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
  }

  trait Codec[A] extends Encoder[A] with Decoder[A]

  val aCodecInvariantFunctor =
    new InvariantFunctor[Codec] {
      override def imap[A, B](fa: Codec[A])(f: A => B)(g: B => A): Codec[B] =
        new Codec[B] {
          override def toString(b: B): String = fa.toString(g(b))
          override def fromString(s: String): B = f(fa.fromString(s))
        }
    }

  ////

  def main(args: Array[String]): Unit = {
    println(aDecoderForInner.fromString("12"))
    println(aDecoderForOuter.fromString("23"))

    println(aEncoderForInner.toString(Inner(7)))
    println(aEncoderForOuter.toString(Outer(Inner(8))))
  }

}
