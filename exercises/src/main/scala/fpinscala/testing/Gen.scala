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

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop
}

object Prop {
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop) = Prop {
      (m, n, rng) =>
        run(m, n, rng) match {
          case Passed => p.run(m, n, rng)
          case x => x
        }
    }

    def ||(p: Prop) = Prop {
      (m, n, rng) =>
        run(m, n, rng) match {
          // In case of failure, run the other prop.
          case Falsified(msg, _) => p.tag(msg).run(m, n, rng)
          case x => x
        }
    }

    def tag(msg: String) = Prop {
      (m, n, rng) =>
        run(m, n, rng) match {
          case Falsified(e, c) => Falsified(msg + "\n" + e, c)
          case x => x
        }
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) =>
      val casesPerSize = (n + (m - 1)) / m
      val props: Stream[Prop] =
        Stream.from(0).take((n min m) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(m, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def check: Boolean = ???

}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State(RNG.sequence(List.fill(n)(a.sample.run))))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

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
case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def unsized = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))

}

case class SGen[+A](g: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen { g(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap { f(_).g(n) }
    }
    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))

}
