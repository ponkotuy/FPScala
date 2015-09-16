package chap5

object Exe3 extends App {
  assert(Stream(1, 2, 3).takeWhile(_ < 3).toList == List(1, 2))
}
