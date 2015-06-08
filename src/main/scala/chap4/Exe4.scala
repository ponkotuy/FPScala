package chap4

object Exe4 extends App {
  def sequence[A](xs: List[Option[A]]): Option[List[A]] = {
    xs.foldRight(Some(Nil): Option[List[A]]) { (x, opts) =>
      Exe3.map2(opts, x) { (ys, y) => y :: ys }
    }
  }
  assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  assert(sequence(List(None, Some(1))) == None)
}
