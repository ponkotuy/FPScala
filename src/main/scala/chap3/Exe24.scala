package chap3

import scala.annotation.tailrec

object Exe24 extends App {
  @tailrec def forAll(as: List[Boolean]): Boolean = as match {
    case Cons(x, xs) => if(x) forAll(xs) else false
    case Nil => true
  }
  @tailrec def forAny(as: List[Boolean]): Boolean = as match {
    case Cons(x, xs) => if(x) true else forAny(xs)
    case Nil => false
  }

  @tailrec def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =sup match {
    case Cons(_, xs) =>
      if(forAll(List.zipWith(sup, sub)(_ == _))) true
      else hasSubsequence(xs, sub)
    case Nil => false
  }

  assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
  assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
  assert(!hasSubsequence(List(1, 2, 3, 4), List(1, 3)))
}
