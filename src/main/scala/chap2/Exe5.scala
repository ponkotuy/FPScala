package chap2

object Exe5 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
