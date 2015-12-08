package chap6

trait RNG {
  def nextInt: (Int, RNG)

  /** Exe6.1 */
  def nonNegativeInt: (Int, RNG) = {
    val (n, nextRNG) = nextInt
    (n & Int.MaxValue, nextRNG)
  }

  /** Exe6.2 */
  def nextDouble: (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt
    val value = n / (Int.MaxValue.toDouble + 1.0)
    (value, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def int: Rand[Int] = _.nextInt
  /** Exxe6.5 */
  def double: Rand[Double] = map(rng => rng.nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1.0))

  def unit[A](a: A): Rand[A] = { rng => (a, rng) }

  /** Exe6.6 */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    fs match {
      case x :: xs =>
        val (a, rng2) = x(rng)
        val (as, rngLast) = sequence(xs)(rng2)
        (a :: as, rngLast)
      case _ => (Nil, rng)
    }
  }
}
