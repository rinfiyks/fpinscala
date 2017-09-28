package fpinscala.laziness

import Stream.cons
import Stream.empty
sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: l)
      case _ => l
    }
    go(this, List.empty).reverse

  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](p: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(p(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def mapViaUnfold[B](p: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some((p(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWithViaUnfold[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWithViaUnfold(s2)((_, _))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    } append Empty

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = cons(a, s)
    return s
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a + b))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n) { case (a) => Some((a, a + 1)) }

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some(1, 1))

}

object StreamTester {
  def main(args: Array[String]): Unit = {
    //    val s = Stream("a1", "b1", "c2", "d2")
    //    println(s.append(Stream("asd", "dfdfgdfg")).toList)
    //    println(s.forAll(x => x.length == 2))
    val x = Stream.fromViaUnfold(12).take(10)
    val y = Stream.fromViaUnfold(30).take(10)
    //    println(x.zipWithViaUnfold(y)((a, b) => a.toString() + " " + b.toString()).toList)
    println(x.startsWith(y))
    println(Stream(1, 2, 3).startsWith(Stream(1, 2)))
  }
}