package chap2

import scala.annotation.tailrec

object Exe2 extends App {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec def f(xs: Array[A]): Boolean = {
      if(xs.length >= 2) {
        if(ordered(xs(0), xs(1))) f(xs.tail) else false
      } else true
    }
    f(as)
  }

  def intOrder(x: Int, y: Int) = x < y

  assert(isSorted(Array(1), intOrder))
  assert(isSorted(Array(1, 2, 3), intOrder))
  assert(!isSorted(Array(1, 2, 0), intOrder))

  @tailrec def isSortedBetter[A](as: Seq[A])(implicit ordering: Ordering[A]): Boolean = {
    if(as.length >= 2) {
      if(ordering.lt(as(0), as(1))) isSortedBetter(as.tail) else false
    } else true
  }

  assert(isSortedBetter(Array(1)))
  assert(isSortedBetter(List(1, 2, 3)))
  assert(!isSortedBetter(Vector(1, 2, 0)))
}
