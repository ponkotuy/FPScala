package chap3

object Exe23 extends App {
  assert(List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))
}
