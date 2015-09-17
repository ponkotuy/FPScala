package chap5

object Exe6 extends App {
  import Stream._
  assert(empty.headOption2 == None)
  val stream: Stream[Int] = cons(1, stream.map(_ + 1))
  assert(stream.headOption2 == Some(1))
}
