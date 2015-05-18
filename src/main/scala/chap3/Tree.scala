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
}
