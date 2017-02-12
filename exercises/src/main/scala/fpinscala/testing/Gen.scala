package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

object GenTester {

  private def multiRun[A](n: Int, s: Gen[A]): List[A] = {
    @annotation.tailrec
    def go(n: Int, simple: RNG, acc: List[A]): List[A] = {
      if (n == 0) acc
      else {
        val r: (A, RNG) = s.sample.run(simple)
        go(n - 1, r._2, r._1 +: acc)
      }
    }
    go(n, RNG.Simple(0), List())
  }

  def main(args: Array[String]): Unit = {
    val a: Gen[Double] = Gen.weighted((Gen.double, 1), (Gen.double2, 9))
    val l: List[Double] = multiRun(10000, a).filter(_ > 1)
    println(l.length)
  }
}

sealed trait Result {
  def isFalisfied: Boolean
}

case object Success extends Result {
  def isFalisfied = false
}

case class Failure(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalisfied = true
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]

  case class Prop(run: TestCases => Result)

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

  def check: Boolean = ???

  def &&(p: Prop): Prop = ???
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State(RNG.sequence(List.fill(n)(a.sample.run))))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  def double2: Gen[Double] =
    Gen(State(RNG.double).map(_ + 1))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Ratio = g1._2.abs / (g1._2.abs + g2._2.abs) // not the best solution because it might overflow
    double.flatMap(x => { if (x < g1Ratio) g1._1 else g2._1 })
  }

}

//trait Gen[A] {
//  def map[A, B](f: A => B): Gen[B] = ???
//  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
//}

case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

}

trait SGen[+A] {

}
