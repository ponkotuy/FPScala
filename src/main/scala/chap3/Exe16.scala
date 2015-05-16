package chap3

/** Exercise 2.16 - 2.18 */
object Exe16 extends App {
  def increments(as: List[Int]): List[Int] =
    List.foldRight[Int, List[Int]](as, Nil) { (x, xs) =>
      Cons(x + 1, xs)
    }

  assert(increments(List(1, 2, 3)) == List(2, 3, 4))
}
