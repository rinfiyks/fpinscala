package fpinscala.iomonad.io

import fpinscala.iomonad.Monad

object IO2c {

  import fpinscala.parallelism.Nonblocking._

  /*
   * We've solved our first problem of ensuring stack safety, but we're still
   * being very inexplicit about what sort of effects can occur, and we also
   * haven't found a way of describing asynchronous computations. Our `Suspend`
   * thunks will just block the current thread when run by the interpreter.
   * We could fix that by changing the signature of `Suspend` to take a `Par`.
   * We'll call this new type `Async`.
   */

  sealed trait Async[A] { // will rename this type to `Async`
    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)

    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends Async[A]

  case class Suspend[A](resume: Par[A]) extends Async[A] // notice this is a `Par`
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  object Async extends Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)

    def flatMap[A, B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
  }

  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  @annotation.tailrec def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }
  }

  // The fact that `run` only uses the `unit` and `flatMap` functions of
  // `Par` is a clue that choosing `Par` was too specific of a choice,
  // this interpreter could be generalized to work with any monad.
}


