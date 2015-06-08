package chap4

object Exe3 extends App {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      x <- a
      y <- b
    } yield f(x, y)
  }

  assert(map2(Some(1), Some(2))(_ + _) == Some(3))
  assert(map2(Some(1), None)(_ + _) == None)
  assert(map2(None: Option[Int], None)(_ + _) == None)
}
