package chap3

object Exe4 extends App {
  assert(List.drop(List(1, 2, 3, 4), 2) == List(3, 4))
  assert(List.drop(Nil, 1) == Nil)
}
