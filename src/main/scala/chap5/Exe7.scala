package chap5

object Exe7 extends App {
  import Stream._
  val stream: Stream[Int] = cons(1, stream.map(_ + 1))
  assert(stream.filter(_ % 2 == 0).take(3).toList == List(2, 4, 6))
  assert(Stream(1, 2, 3).append(4).toList == List(1, 2, 3, 4))
  assert(Stream(1, 2, 3, 4).append({throw new RuntimeException(""); 5}).take(3).toList == List(1, 2, 3))
  assert(Stream(1, 2, 3).flatMap { n => Stream(n, n + 1) }.toList == List(1, 2, 2, 3, 3, 4))
}
