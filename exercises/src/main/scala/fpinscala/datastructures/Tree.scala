package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((a, b) => (a max b) + 1)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}

object TreeTester {
  def main(args: Array[String]): Unit = {

    var t: Tree[Int] = Branch(Branch(Leaf(8), Branch(Leaf(4), Leaf(2))), Leaf(1))
    var u: Tree[Int] = Branch(Branch(Branch(Leaf(4), Leaf(2)), Leaf(8)), Leaf(1))

    /*     t
     *    / \
     *   _   1
     *  / \
     * 8   _
     *    / \
     *   4   2
     */

    println(Tree.map2(t)(a => a * a))

  }
}
