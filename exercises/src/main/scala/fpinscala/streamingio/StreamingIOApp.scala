package fpinscala.streamingio

import java.io.File
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.iomonad.IO
import fpinscala.iomonad.io.IO3
import fpinscala.parallelism.Nonblocking.Par
import fpinscala.streamingio.SimpleStreamTransducers.Process
import fpinscala.streamingio.SimpleStreamTransducers.Process._

object StreamingIOApp extends App {
  implicit val es = Executors.newFixedThreadPool(4)

  val res1 = count(Stream(0, 0, 0, 0, 0))
  println(res1.toList)

  val res2: Stream[Double] = mean(Stream(10.0, 20.0, 40.0))
  println(res2.toList)

  val res3 = sumViaLoop(Stream(1, 2, 3, 4, 5, 6))
  println(res3.toList)

  val res4 = countViaLoop(Stream(10, 10, 10, 10, 10, 10)).filter(_ % 2 == 1)
  println(res4.toList)

  val pipe: Process[Int, Int] = countViaLoop |> filter(_ % 2 == 1)
  val res5 = pipe(Stream(10, 10, 10, 10, 10, 10))
  println(res5.toList)

  val res6 = (count |> exists(_ % 10 == 0)) (Stream.continually(1).take(20))
  println(res6.toList)

  val inputStream1 = getClass.getResource("/fpinscala/streamingio/testfile1.txt").openStream()
  val io1: IO[Boolean] = processFile(inputStream1, count |> exists(_ >= 20), false)(_ || _)
  val res7: Boolean = unsafePerformIO(io1)
  println(res7)

  val inputStream2 = getClass.getResource("/fpinscala/streamingio/testfile1.txt").openStream()
  val io2 = processFile(inputStream2,
    lift((line: String) => line.split(" ").last.toInt) |> filter(_ % 2 == 0),
    List.empty[Int])((b, a) => a +: b)
  val res8: List[Int] = unsafePerformIO(io2)
  println(res8)

  val inputStream3 = getClass.getResource("/fpinscala/streamingio/fahrenheit_list.txt").openStream()
  val io3 = processFile(inputStream3,
    lift(identity),
    List.empty[String])((b, a) => a +: b)
  val res9: List[String] = unsafePerformIO(io3).reverse
  println(res9)
  val io4 = toFile(new File("/tmp/f_to_c.txt"), fahrenheitToCelsuis, res9)
  unsafePerformIO(io4)

  def unsafePerformIO[A](io: IO[A])(implicit E: ExecutorService): A =
    Par.run(E) {
      IO3.run(io)(IO3.parMonad)
    }
}
