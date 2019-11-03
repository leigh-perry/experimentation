package exp

import scala.annotation.tailrec

object Cps {

  def printTree1(ot: Option[Tree]): Unit =
    ot match {
      case Some(t) =>
        printTree1(t.left)
        output(t.value)
        printTree1(t.right)
      case None => ()
    }

  def printTree2(ot: Option[Tree], c: () => Unit): Unit =
    //output(s"[$ot]")
    ot match {
      case Some(t) =>
        printTree2(t.left, () => {
          output(t.value)
          printTree2(t.right, c)
        })
      case None =>
        c()
    }

  final case class ContPrintTree(tt: Tree, followedBy: Option[ContPrintTree])

  @tailrec
  def printTree3(ot: Option[Tree], c: Option[ContPrintTree]): Unit =
    //output(s"[$ot]")
    ot match {
      case Some(t) =>
        printTree3(t.left, Some(ContPrintTree(t, c)))
      case None =>
        c match {
          case Some(cc) =>
            output(cc.tt.value)
            printTree3(cc.tt.right, cc.followedBy)
          case None =>
        }
    }

  def printTree4(aot: Option[Tree], ac: Option[ContPrintTree]): Unit = {
    var ot = aot
    var c = ac
    while (true) {
      ot match {
        case Some(t) => {
          ot = t.left
          c = Some(ContPrintTree(t, c))
        }
        case None =>
          c match {
            case Some(cc) =>
              output(cc.tt.value)
              ot = cc.tt.right
              c = cc.followedBy
            case None =>
              return
          }
      }
    }
  }

  ////

  def output(s: String) = {
    print(" " * s.length)
    println(s)
  }

  ////

  def factorial1(n: Int): Int =
    if (n < 2) n
    else n * factorial1(n - 1)

  @tailrec
  def factorial2(n: Int, acc: Int): Int =
    if (n < 2) n * acc
    else {
      //val fac = factorial1(n - 1)
      //n * fac
      factorial2(n - 1, n * acc)
    }

  @tailrec
  def factorial3(n: Int, c: Int => Int): Int =
    if (n < 2) c(1)
    else factorial3(n - 1, y => c(y * n))

  ////

  sealed trait ContFactorial
  object ContFactorial {
    final case class Multiply(n: Int, followedBy: ContFactorial) extends ContFactorial
    final case object Identity extends ContFactorial
  }

  def eval(c: ContFactorial, n: Int): Int =
    c match {
      case ContFactorial.Multiply(nn, followedBy) => eval(followedBy, n * nn)
      case ContFactorial.Identity => n
    }

  @tailrec
  def factorial4(n: Int, c: ContFactorial): Int =
    if (n < 2) eval(c, 1)
    else factorial4(n - 1, ContFactorial.Multiply(n, c))

  ////

  def main(args: Array[String]): Unit = {
    val tree =
      Tree(
        Some(Tree(Some(Tree(None, "l1l2", None)), "l1", Some(Tree(None, "l1l1", None)))),
        "x",
        Some(Tree(Some(Tree(None, "r1r1", None)), "r1", Some(Tree(None, "r1r2", None))))
      )

    println("==== printTree1 ====")
    printTree1(Some(tree))
    println("==== printTree2 ====")
    printTree2(Some(tree), () => ())
    println("==== printTree3 ====")
    printTree3(Some(tree), None)
    println("==== printTree4 ====")
    printTree4(Some(tree), None)

    println("==== factorial1 ====")
    val factVal = 12
    println(factorial1(factVal))
    println("==== factorial2 ====")
    println(factorial2(factVal, 1))
    println("==== factorial3 ====")
    println(factorial3(factVal, identity))
    println("==== factorial4 ====")
    println(factorial4(factVal, ContFactorial.Identity))
  }

  final case class Tree(left: Option[Tree], value: String, right: Option[Tree])
}
