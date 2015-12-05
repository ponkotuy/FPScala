package chap5

object Exe10 extends App {
  import Stream._
  val fibs: Stream[Int] = cons(0, cons(1, fibs.zip(fibs.tail).map { case (x, y) => x + y }))
  assert(fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
}
