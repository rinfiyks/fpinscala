package fpinscala.iomonad.io

import scala.io.StdIn.readLine

object IO0 {

  /*
Our first attempt at data type for representing computations that
may perform I/O. Has a simple 'interpreter' baked in--the `run`
function, which just returns `Unit`.
   */

  trait IO {
    self =>
    def run(): Unit

    def ++(io: IO): IO = new IO {
      def run(): Unit = {
        self.run()
        io.run()
      }
    }
  }

  object IO {
    def empty: IO = new IO {
      def run(): Unit = ()
    }
  }

  /*
The API of this `IO` type isn't very useful.  Not many operations
(it is only a monoid), and not many laws to help with reasoning. It
is completely _opaque_. Also cannot represent _input_ effects, like
reading from console, for instance:
   */

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  // Ordinary code with side effects
  def converter(): Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

  // A pure version is not possible!
  /*
  def converter: IO = {
    val prompt: IO = PrintLine("Enter a temperature in degrees fahrenheit: ")
    // now what ???
  }
  */
}