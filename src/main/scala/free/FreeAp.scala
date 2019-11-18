package free

import cats.effect.IO
import cats.{ ~>, Applicative }
import cats.syntax.apply._

sealed trait FreeAp[F[_], A] {
  import FreeAp._

  def foldMap[G[_]: Applicative](nt: F ~> G): G[A] =
    this match {
      case Pure(a) => Applicative[G].pure(a)

      case Ap(fa, f) => // f: FreeAp[F, A => B]
        val ga: G[A] = fa.asInstanceOf[FreeAp[F, A]].foldMap(nt)
        Applicative[G].ap(f.foldMap(nt))(ga.asInstanceOf[G[Any]])

      case Suspend(fa) => nt(fa)
    }
}

object FreeAp {
  def liftM[F[_], A](fa: F[A]): FreeAp[F, A] = Suspend(fa)

  final case class Pure[F[_], A](a: A) extends FreeAp[F, A]
  final case class Ap[F[_], A, B](fa: FreeAp[F, A], f: FreeAp[F, A => B]) extends FreeAp[F, B]
  final case class Suspend[F[_], A](fa: F[A]) extends FreeAp[F, A]

  implicit def applicativeForFreeAp[F[_]]: Applicative[FreeAp[F, *]] =
    new Applicative[FreeAp[F, *]] {
      override def pure[A](x: A): FreeAp[F, A] =
        Pure(x)
      override def ap[A, B](ff: FreeAp[F, A => B])(fa: FreeAp[F, A]): FreeAp[F, B] =
        // naive implementation – NOTE: not strictly associative until interpreted
        //Ap(fa, ff)
        // --------------
        // optimised implementation by applying laws
        //  pure id <*> v = v                            -- Identity
        //  pure f <*> pure x = pure (f x)               -- Homomorphism
        //  u <*> pure y = pure ($ y) <*> u              -- Interchange
        //    Ap(u, Pure(x)) must always equal Ap(Pure(f => f(x)), u)
        //  pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
        //    Ap(Ap(Ap(Pure(compose), u), v), w) must be equal to Ap(u, Ap(v, w))
        (ff, fa) match {
          case (Pure(f), Pure(x)) =>
            Pure(f(x))
          case (u, Pure(x)) =>
            // Interchange Ap(u, Pure(x)) must always equal Ap(Pure(f => f(x)), u)
            Ap(
              Pure((f: A => B) => f(x)).asInstanceOf[FreeAp[F, Any]],   // TODO how to eliminate casts?
              u.asInstanceOf[FreeAp[F, Any => B]]
            )
          case _ =>
            Ap(fa, ff)
        }
    }
}

//// sample app

sealed trait ApOps[A]

object ApOps {
  final case class Read1(file: String) extends ApOps[Int]
  final case class Read2(file: String) extends ApOps[Int]
}

object TestFreeAp {
  val interpreter: ApOps ~> IO =
    new (ApOps ~> IO) {
      override def apply[A](fa: ApOps[A]): IO[A] =
        fa match {
          case ApOps.Read1(file) =>
            IO {
              println(fa)
              1234
            }
          case ApOps.Read2(file) =>
            IO {
              println(fa)
              2345
            }
        }
    }

  def main(args: Array[String]): Unit = {
    val program: FreeAp[ApOps, Int] =
      (FreeAp.liftM(ApOps.Read1("filename1")), FreeAp.liftM(ApOps.Read2("filename2")))
        .mapN(_ + _)

    val r: IO[Int] = program.foldMap(interpreter)
    println(r.unsafeRunSync())
  }
}
