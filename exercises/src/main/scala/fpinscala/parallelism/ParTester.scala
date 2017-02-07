package fpinscala.parallelism

import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

import Nonblocking._
import Nonblocking.Par._

object ParTester {

  def main(args: Array[String]): Unit = {
    val es: ExecutorService = Executors.newFixedThreadPool(2)

    val p1 = parFilter(List.range(1, 10))(_ % 2 == 0)
    println("Finished creating p1")
    val result = run(es)(p1)
    println(result)
    es.shutdown()
  }
}
