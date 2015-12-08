package chap6

import scala.util.Random

object Exe6 extends App {
  import chap6.RNG._
  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  val rng = new SimpleRNG(Random.nextLong())
  assert(Exe3.intDouble(rng) == randIntDouble(rng))
}
