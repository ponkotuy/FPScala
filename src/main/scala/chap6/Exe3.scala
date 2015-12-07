package chap6

object Exe3 extends App {
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, r1) = rng.nextInt
    val (double, r2) = r1.nextDouble
    ((int, double), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (double, r1) = rng.nextDouble
    val (int, r2) = r1.nextInt
    ((double, int), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = rng.nextDouble
    val (d2, r2) = r1.nextDouble
    val (d3, r3) = r2.nextDouble
    ((d1, d2, d3), r3)
  }
}
