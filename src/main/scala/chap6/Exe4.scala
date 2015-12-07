package chap6

import scala.util.Random

object Exe4 extends App {
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if(count <= 0) (Nil, rng)
    else {
      val (n, nextRNG) = rng.nextInt
      val (result, lastRNG) = ints(count - 1)(nextRNG)
      (n :: result, lastRNG)
    }

  println(ints(3)(SimpleRNG(Random.nextLong())))
}
