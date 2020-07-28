package exp

import cats.Alternative

object AlternativeEtc {
  trait Decoder[A] {
    def decode(in: String): Either[Throwable, A]
  }
  object Decoder {
    def from[A](f: String => Either[Throwable, A]): Decoder[A] =
      new Decoder[A] {
        def decode(in: String) = f(in)
      }
  }

  import cats.implicits._

  implicit val decoderAlternative: Alternative[Decoder] =
    new Alternative[Decoder] {
      override def pure[A](x: A): Decoder[A] =
        Decoder.from(_ => x.asRight)

      override def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]): Decoder[B] =
        Decoder.from {
          s =>
            fa.decode(s)
              .ap(ff.decode(s))
        }

      override def empty[A]: Decoder[A] =
        Decoder.from(_ => (new RuntimeException).asLeft)

      override def combineK[A](x: Decoder[A], y: Decoder[A]): Decoder[A] =
        Decoder.from(s => x.decode(s).orElse(y.decode(s)))
    }

  def main(args: Array[String]): Unit = {
    import cats.Alternative
    import cats.implicits._

    println(Alternative[Vector].empty[Int]) // Vector()

    val v78 = 7.pure[Vector] <+> 8.pure[Vector]
    println(v78) // Vector(7, 8)

    val double: Int => Int = _ * 2
    val addFive: Int => Int = _ + 5

    val funcs: Vector[Int => Int] = double.pure[Vector] <+> addFive.pure[Vector]
    println(funcs ap v78) // Vector(14, 16, 12, 13)

    ////

    def parseInt(s: String): Either[Throwable, Int] = Either.catchNonFatal(s.toInt)

    def parseIntFirstCharDoubler(s: String): Either[Throwable, Int] =
      Either.catchNonFatal(2 * Character.digit(s.charAt(0), 10))

    // Try first parsing the whole, then just the first character.
    val decoder: Decoder[Int] = Decoder.from(parseInt) <+> Decoder.from(parseIntFirstCharDoubler)

    println(decoder.decode("555")) // Right(555)
    println(decoder.decode("5a")) // Right(10)

    ////

    def requestResource(a: Int): Either[(Int, String), (Int, Long)] =
      if (a % 4 == 0) Left((a, "Bad request"))
      else if (a % 3 == 0) Left((a, "Server error"))
      else Right((a, 200L))

    val vec: Vector[Either[(Int, String), (Int, Long)]] =
      (requestResource _).pure[Vector].ap(Vector(5, 6, 7, 99, 1200, 8, 22))

    val partitionedResults: (Vector[(Int, String)], Vector[(Int, Long)]) =
      vec.separate

    // (Vector((6,Server error), (99,Server error), (1200,Bad request), (8,Bad request)),Vector((5,200), (7,200), (22,200)))
    println(partitionedResults)

    ////

    def getRegionAndDistrict(pkey: Int): (Int, Vector[Int]) =
      (5 * pkey, (double.pure[Vector] <+> addFive.pure[Vector]) ap pkey.pure[Vector])

    println(getRegionAndDistrict(2)) // (10,Vector(4, 7))

    val regionsWithDistricts: Vector[(Int, Vector[Int])] =
      (getRegionAndDistrict _).pure[Vector] ap Vector(5, 6, 7, 97, 1200, 8, 25)

    // Vector((25,Vector(10, 10)), (30,Vector(12, 11)), (35,Vector(14, 12)), (485,Vector(194, 102)), (6000,Vector(2400, 1205)), (40,Vector(16, 13)), (125,Vector(50, 30)))
    println(regionsWithDistricts)

    val separated: (Vector[Int], Vector[Vector[Int]]) = regionsWithDistricts.separate // catsStdBitraverseForTuple2
    val regionIds = separated._1
    println(regionIds)  // Vector(25, 30, 35, 485, 6000, 40, 125)

    val districtIds = separated._2.flatten
    println(districtIds)  // Vector(10, 10, 12, 11, 14, 12, 194, 102, 2400, 1205, 16, 13, 50, 30)
  }
}
