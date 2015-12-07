package chap6

import scala.util.Random

object Exe2 extends App {
  (1 to 1000).foreach { _ =>
    val rng = SimpleRNG(Random.nextLong())
    val (x, _) = rng.nextDouble
    assert(0.0 <= x)
  }
}
