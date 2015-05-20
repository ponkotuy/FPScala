package chap3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /** Exercise 25 */
  def size(as: Tree[_]): Int = as match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /** Exercise 26 */
  def maximum(as: Tree[Int]): Int = as match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  /** Exercise 27 */
  def depth(as: Tree[_]): Int = as match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  /** Exercise 28 */
  def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /** Exercise 29 */
  def fold[A, B](as: Tree[A])(f: A => B)(g: (B, B) => B): B = as match {
    case Leaf(x) => f(x)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }
}
