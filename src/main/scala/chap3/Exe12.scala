package chap3

object Exe12 extends App {
  assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
  assert(List.reverse(Nil) == Nil)
}
