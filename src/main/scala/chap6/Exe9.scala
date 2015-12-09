package chap6

import scala.util.Random

object Exe9 extends App {
  import RNG._
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    map(rb) { b =>
      f(a, b)
    }
  }

  val rng = new SimpleRNG(Random.nextLong())

  foreach(map2(int, double)((_, _)))(println)(rng)
  assert(map2(int, double)((_, _))(rng) == RNG.map2(int, double)((_, _))(rng))
}
