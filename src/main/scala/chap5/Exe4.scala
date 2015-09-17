package chap5

object Exe4 extends App {
  import Stream._
  assert(Stream(1, 2, 3).forAll(_ < 4))
  val stream: Stream[Int] = cons(1, stream.map(_ + 1))
  assert(!stream.forAll(_ < 4))
}
