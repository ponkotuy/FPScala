package chap5

object Exe8 extends App {
  assert(Stream.constant("a").take(3).toList == List("a", "a", "a"))
}
