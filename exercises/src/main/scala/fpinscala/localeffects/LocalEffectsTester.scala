package fpinscala.localeffects

object LocalEffectsTester extends App {

  val l = List(6, 13, 75, 24, 76, 21, 5, 3)
  println(Mutable.quicksort(l))

  val p = new RunnableST[(Int, Int)] {
    def apply[S] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }

  println(ST.runST(p))

}
