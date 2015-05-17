package chap3

object Exe22 extends App {
  def sums(as: List[Int], bs: List[Int]): List[Int] =  (as, bs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sums(xs, ys))
    case _ => Nil
  }
  assert(sums(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
}
