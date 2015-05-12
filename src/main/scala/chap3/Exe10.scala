package chap3

object Exe10 extends App {
  assert(List.foldLeft(List(1, 2, 3, 4), 0)(_ + _) == 10)
  assert(List.foldLeft(Nil, 1){ (x: Int, y: Int) => x + y } == 1)
}
