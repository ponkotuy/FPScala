package chap6

import scala.util.Random

object Exe5 extends App {
  val rng = SimpleRNG(Random.nextLong())
  assert(RNG.double(rng) == rng.nextDouble)
}
