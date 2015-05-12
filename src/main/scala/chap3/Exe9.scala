package chap3

object Exe9 extends App {
  def length[A](xs: List[A]): Int = {
    List.foldRight(xs, 0)((_, x) => x + 1)
  }
  assert(length(List(1, 2, 3)) == 3)
  assert(length(Nil) == 0)
}
