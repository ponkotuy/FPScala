package chap6

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(nextSeed)
    val n = (nextSeed >>> 16).toInt
    (n, nextRNG)
  }
}
