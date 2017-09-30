package fpinscala.streamingio

import SimpleStreamTransducers.Process

object StreamingIOApp extends App {

  val res1 = Process.count2(Stream(0, 0, 0, 0, 0))
  println(res1.toList)

  val res2: Stream[Double] = Process.mean(Stream(10.0, 20.0, 40.0))
  println(res2.toList)

  val res3 = Process.sumViaLoop(Stream(1, 2, 3, 4, 5, 6))
  println(res3.toList)

  val res4 = Process.countViaLoop(Stream(10, 10, 10, 10, 10))
  println(res4.toList)
}
