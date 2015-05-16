package chap3

object Exe20 extends App {
  assert(List.flatMap(List(1, 2, 3)) { x => List(x, x) } == List(1, 1, 2, 2, 3, 3))
}
