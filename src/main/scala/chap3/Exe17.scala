package chap3

object Exe17 extends App {
  def toStrings(as: List[Double]): List[String] =
    List.foldRight[Double, List[String]](as, Nil) { (x, xs) =>
      Cons(x.toString, xs)
    }

  assert(toStrings(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
}
