package chap3

object Exe5 extends App {
  assert(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3) == List(3, 4, 5))
  assert(List.dropWhile(List(1, 2, 3), (x: Int) => x > 0) == Nil)
  assert(List.dropWhile(Nil, (x: Int) => x > 0) == Nil)
}
