package support

import cats.syntax.option._

final class OptionalOps(val bool: Boolean) extends AnyVal {
  def optionf[A, B](a: A, f: A => B): Option[B] =
    if (bool) f(a).some else None

  def option[A](a: A): Option[A] =
    if (bool) a.some else None
}

trait ToOptionalOps {
  implicit def `Ops for Optional`[A](e: Boolean): OptionalOps =
    new OptionalOps(e)
}

trait OptionalSupport
  extends ToOptionalOps

object optionalinstances
  extends OptionalSupport
