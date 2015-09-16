package chap5

object Exe1 extends App {
  import Stream._
  assert(cons(1, cons(2, cons(3, empty))).toList == List(1, 2, 3))
}
