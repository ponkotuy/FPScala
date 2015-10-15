package chap5

object Exe6 extends App {
  import Stream._
  assert(empty.headOption2.isEmpty)
  val stream: Stream[Int] = cons(1, stream.map(_ + 1))
  assert(stream.headOption2.contains(1))
}
