package chap3

object Exe21 extends App {
  def filter[A](as: List[A])(f: A => Boolean): List[A] = List.flatMap(as) { x =>
    if(f(x)) List(x) else Nil
  }

  assert(filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4))
}
