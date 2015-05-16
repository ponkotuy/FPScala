package chap3


object Exe19 extends App {
  assert(List.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4))
}
