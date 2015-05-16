package chap3

object Exe18 extends App {
  assert(List.map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
  assert(List.map(List(1.0, 2.0, 3.0))(_.toString) == List("1.0", "2.0", "3.0"))
}
