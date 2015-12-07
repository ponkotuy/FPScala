package chap6

import scala.util.Random

object Exe1 extends App {
  (1 to 1000).foreach { _ =>
    val rng = SimpleRNG(Random.nextLong())
    val value = rng.nonNegativeInt._1
    assert(0 <= value)
  }
}
