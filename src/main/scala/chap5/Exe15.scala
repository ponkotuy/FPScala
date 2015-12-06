package chap5

object Exe15 extends App {
  assert(Stream(1, 2, 3).tails.map(_.toList).toList == List(List(1, 2, 3), List(2, 3), List(3), Nil))
}
