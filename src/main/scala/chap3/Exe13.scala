package chap3

object Exe13 extends App {
  /**
   * from Real World Haskell 4.6.1
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    List.foldRight[A, B => B](as, identity) { (a, x) =>
      { b: B => x(f(b, a)) }
    }(z)
  }
  assert(foldLeft[Int, List[Int]](List(1, 2, 3), Nil)((b, a) => Cons(a, b)) == List(3, 2, 1))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    List.foldLeft[A, B => B](as, identity) { (x, a) =>
      { b: B => x(f(a, b)) }
    }(z)
  }
  assert(foldRight[Int, List[Int]](List(1, 2, 3), Nil)((a, b) => Cons(a, b)) == List(1, 2, 3))
}
