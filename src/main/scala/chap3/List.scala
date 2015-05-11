package chap3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil else Cons(as.head, apply(as.tail:_*))
  }

  /** Exercise 3.2 */
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new UnsupportedOperationException
    case Cons(_, ys) => ys
  }

  /** Exercise 3.3 */
  def setHead[A](xs: List[A], head: A): List[A] = xs match {
    case Nil => throw new UnsupportedOperationException
    case Cons(_, ys) => Cons(head, ys)
  }

  /** Exercise 3.4 */
  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case Cons(_, ys) if n > 0 => drop(ys, n - 1)
    case ys: Cons[A] => ys
  }

  /** Exercise 3.5 */
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(y, ys) if f(y) => dropWhile(ys, f)
    case ys: Cons[A] => ys
  }
}
