package chap5

object Exe2 extends App {
  assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
  assert(Stream(1, 2, 3).drop(1).toList == List(2, 3))
}
