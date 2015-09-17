package chap5

object Exe5 extends App {
  import Stream._
  assert(Stream(1, 2, 3, 4).takeWhile2(_ < 4).toList == List(1, 2, 3))
  val stream: Stream[Int] = cons(1, stream.map(_ + 1))
  assert(stream.takeWhile2(_ < 6).toList == List(1, 2, 3, 4 ,5))
}
