package chap3

object Exe15 extends App {
  assert(List.concat(List(List(1), Nil, List(2, 3))) == List(1, 2, 3))
}
