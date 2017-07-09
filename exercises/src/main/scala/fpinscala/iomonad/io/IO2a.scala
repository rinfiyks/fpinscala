package fpinscala.iomonad.io

import fpinscala.iomonad.Monad

object IO2a {

  /*
  The previous IO representation overflows the stack for some programs.
  The problem is that `run` call itself recursively, which means that
  an infinite or long running IO computation will have a chain of regular
  calls to `run`, eventually overflowing the stack.

  The general solution is to make the `IO` type into a data type that we
  interpret using a tail recursive loop, using pattern matching.
  */

  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f) // we do not interpret the `flatMap` here, just return it as a value
    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends IO[A]

  case class Suspend[A](resume: () => A) extends IO[A]

  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] { // Notice that none of these operations DO anything
    def unit[A](a: => A): IO[A] = Return(a)

    def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f

    def suspend[A](a: => IO[A]): IO[A] =
      Suspend(() => ()).flatMap { _ => a }
  }

  def printLine(s: String): IO[Unit] =
    Suspend(() => Return(println(s)))

  val p: IO[Nothing] = IO.forever(printLine("Still going..."))

  val actions: Stream[IO[Unit]] =
    Stream.fill(100000)(printLine("Still going..."))
  val composite: IO[Unit] =
    actions.foldLeft(IO.unit(())) { (acc, a) => acc flatMap { _ => a } }

  // There is only one sensible way to implement this as a
  // tail-recursive function, the one tricky case is left-nested
  // flatMaps, as in `((a flatMap f) flatMap g)`, which we
  // reassociate to the right as `a flatMap (ar => f(a) flatMap g)`
  @annotation.tailrec def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}


object IO2aTests {

  import IO2a._

  /*
  Pg 240: REPL session has a typo, should be:

  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b)}
  }

  Note: we could write a little helper function to make this nicer:

  def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap { _ => a }

  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => suspend { a(x).flatMap(b) }
  }
   */

  val f: Int => IO[Int] = (i: Int) => Return(i)

  val g: Int => IO[Int] =
    List.fill(10000)(f).foldLeft(f) {
      (a: Int => IO[Int], b: Int => IO[Int]) => {
        (x: Int) => IO.suspend(a(x).flatMap(b))
      }
    }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}