package chap5

object Exe9 extends App {
  assert(Stream.from(3).take(3).toList == List(3, 4, 5))
}
