package fpinscala.iomonad.io

import fpinscala.iomonad.Monad

import scala.io.StdIn.readLine

object IO1 {

  /*
We need a way for our `IO` actions to yield a result of some
meaningful type. We do this by adding a type parameter to `IO`,
which now forms a `Monad`.
   */

  sealed trait IO[A] {
    self =>
    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] {
        def run: B = f(self.run)
      }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] {
        def run: B = f(self.run).run
      }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run: A = a
    }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a) // syntax for IO { .. }

    def ref[A](a: A): IO[IORef[A]] = IO {
      new IORef(a)
    }

    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO {
        value = a
        a
      }

      def get: IO[A] = IO {
        value
      }

      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }

  }

  // We can now express the example

  def ReadLine: IO[String] = IO {
    readLine
  }

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  import IO0.fahrenheitToCelsius

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  /*                         Some other examples                      */

  import IO._ // import all the `IO` combinators that come from `Monad`

  // An `IO[Unit]` that reads a line from the console and echoes it back.
  val echo: IO[Unit] = ReadLine.flatMap(PrintLine)

  // Parses an `Int` by reading a line from the console.
  val readInt: IO[Int] = ReadLine.map(_.toInt)

  // Parses an `(Int,Int)` by reading two lines from the console.
  val readInts: IO[(Int, Int)] = readInt ** readInt

  // Repeat `converter` 5 times, discarding the results (which are
  // just `Unit`). We can replace `converter` here with any `IO`
  // action we wished to repeat 5 times (ex: `echo` or `readInts`).
  val prompts: IO[Unit] = replicateM_(5)(converter)

  // An `IO[List[String]]` that will read 10 lines from the console and
  // return the list of results.
  val lines: IO[List[String]] = replicateM(10)(ReadLine)

  /*
Larger example using various monadic combinators. Sample run:

The Amazing Factorial REPL, v2.0
q - quit
<number> - compute the factorial of the given number
<anything else> - bomb with horrible error
3
factorial: 6
7
factorial: 5040
q
   */

  val helpstring: String =
    """
      | The Amazing Factorial REPL, v2.0
      | q - quit
      | <number> - compute the factorial of the given number
      | <anything else> - bomb with horrible error
    """.trim.stripMargin

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM(1 to n toStream)(i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = sequence_(
    IO {
      println(helpstring)
    },
    doWhile {
      IO {
        readLine
      }
    } { line =>
      val ok = line != "q"
      when(ok) {
        for {
          n <- factorial(line.toInt)
          _ <- IO {
            println("factorial: " + n)
          }
        } yield ()
      }
    }
  )
}

object IO1Tester extends App {
  IO1.factorialREPL.run
}