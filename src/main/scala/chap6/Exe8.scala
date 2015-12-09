package chap6

import scala.util.Random

object Exe8 extends App {
  import RNG._
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if(i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  val rng = new SimpleRNG(Random.nextLong())

  println(nonNegativeLessThan(100)(rng))
  val seq = RNG.sequence(List.fill(10)(nonNegativeLessThan(1000)))
  val result = map(seq)(_.forall(_ < 1000))
  foreach(seq)(println)(rng)
  foreach(result)(assert)(rng)
}
