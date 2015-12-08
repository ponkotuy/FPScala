package chap6

import scala.util.Random

object Exe7 extends App {
  import chap6.RNG._
  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))
  val rng = new SimpleRNG(Random.nextLong())
  assert(ints(10)(rng) == Exe4.ints(10)(rng))
}
