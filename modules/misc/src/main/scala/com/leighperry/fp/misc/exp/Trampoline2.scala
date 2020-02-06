package exp

import exp.Trampoline2.Step.FlatMap

import scala.annotation.tailrec

// https://free.cofree.io/2017/08/24/trampoline/
object Trampoline2 {

  def unsafeFac(n: Int): Int =
    if (n == 0) 1
    else {
      val prev = unsafeFac(n - 1)
      n * prev
    }

  //// define free monadic trampoline

  sealed trait Step[A] {
    self =>

    def map[B](f: A => B): Step[B] =
      flatMap(a => Step.Pure(f(a)))

    def flatMap[B](f: A => Step[B]): Step[B] =
      FlatMap(self, f)

  }
  object Step {
    final case class Pure[A](a: A) extends Step[A]
    final case class Suspend[A](resume: () => Step[A]) extends Step[A] // TODO just store A?
    final case class FlatMap[A, B](parent: Step[A], f: A => Step[B]) extends Step[B]
  }

  import Step._

  @tailrec
  def run[A](s: Step[A]): A =
    s match {
      case Pure(a) =>
        a
      case Suspend(resume) =>
        run(resume())
      case FlatMap(parent, f) =>
        // [not tail recursive] run(f(run(parent)))
        parent match {
          case Pure(a) =>
            run(f(a))
          case Suspend(resume) =>
            run(resume().flatMap(f))
          case FlatMap(grandparent, parentF) =>
            run(grandparent.flatMap(v => parentF(v).flatMap(f)))
        }
    }

  //// rewrite unsafeFac:
  // If the original function returns an A, the new function should return a Step[A].
  // Each return in the original function should be wrapped in a Pure.
  // Each recursive call in the original function should be wrapped in Suspend.
  // Continuations after the recursive call should be wrapped in a FlatMap.

  def safeFac(n: Int): Step[Int] =
    if (n == 0) Pure(1)
    else
      Suspend(() => safeFac(n - 1))
        .flatMap(p => Pure(n * p))
  // for {
  //   prev <- Suspend(() => Pure(unsafeFac(n - 1)))
  //   r <- Pure(n * prev)
  // } yield r

  ////

  def unsafeFib(n: Int): Int =
    if (n <= 1) n
    else {
      val p1 = unsafeFib(n - 2)
      val p2 = unsafeFib(n - 1)
      p1 + p2
    }

  def safeFib(n: Int): Step[Int] =
    if (n <= 1) Pure(n)
    else {
      for {
        p1 <- Suspend(() => safeFib(n - 2))
        p2 <- Suspend(() => safeFib(n - 1))
      } yield p1 + p2
    }

  ////

  sealed trait Tree[A] {
    def label: A
  }

  object Tree {
    final case class Leaf[A](label: A) extends Tree[A]
    final case class Node[A](label: A, children: List[Tree[A]]) extends Tree[A]
  }

  import Tree._

  def unsafeTreeMap[A, B](tree: Tree[A], f: A => B): Tree[B] =
    tree match {
      case Leaf(a) =>
        Leaf(f(a))
      case Node(a, children) =>
        val b = f(a)
        val treeB = children.map(unsafeTreeMap(_, f))
        Node(b, treeB)
    }

  def safeTreeMap1[A, B](tree: Tree[A], f: A => B): Step[Tree[B]] =
    tree match {
      case Leaf(a) =>
        Pure(Leaf(f(a)))
      case Node(a, children) =>
        for {
          b <- Pure(f(a))
          treeB <- Suspend(() => Pure(children.map(t => run(safeTreeMap1(t, f)))))
        } yield Node(b, treeB)
    }

  ////

  def sequence[A](steps: List[Step[A]]): Step[List[A]] =
    steps.foldRight(Pure(List.empty[A]): Step[List[A]]) {
      (stepA: Step[A], listA: Step[List[A]]) =>
        stepA.flatMap(a => listA.map(l => a :: l))
    }

  def safeTreeMap2[A, B](tree: Tree[A], f: A => B): Step[Tree[B]] =
    tree match {
      case Leaf(a) =>
        Pure(Leaf(f(a)))
      case Node(a, children) =>
        for {
          b <- Pure(f(a))
          treeB <- Suspend(() => sequence(children.map(t => safeTreeMap2(t, f))))
        } yield Node(b, treeB)
    }

  ////

  def main(args: Array[String]): Unit = {
    println(run(safeFac(6)))
    (1 to 10).foreach {
      i =>
        print(s"${run(safeFib(i))} ")
    }
    println()

    val tree =
      Node(
        1,
        List(
          Leaf(2),
          Leaf(3),
          Node(
            4,
            List(Leaf(5), Leaf(6))
          )
        )
      )
    println(run(safeTreeMap1(tree, (_: Int) * 10)))
    println(run(safeTreeMap2(tree, (_: Int) * 10)))
  }
}
