package chap3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
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
  @tailrec def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case Cons(_, ys) if n > 0 => drop(ys, n - 1)
    case ys: Cons[A] => ys
  }

  /** Exercise 3.5 */
  @tailrec def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(y, ys) if f(y) => dropWhile(ys, f)
    case ys: Cons[A] => ys
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /** Exercise 3.10 */
  @tailrec def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /** Exercise 3.11 */
  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  /** Exercise 3.11 */
  def product(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  /** Exercise 3.11 */
  def length(as: List[_]): Int = foldLeft(as, 0)((x, _) => x + 1)

  /** Exercise 3.12 */
  def reverse[A](as: List[A]): List[A] = foldLeft[A, List[A]](as, Nil) { (xs, x) => Cons(x, xs) }

  /** Exercise 3.14 */
  def append[A](as: List[A], x: A): List[A] = foldRight(as, List(x))(Cons.apply)

  /** Exercise 3.15 */
  def concat[A](ass: List[List[A]]): List[A] = {
    foldRight[List[A], List[A]](ass, Nil) { (ys, xs) =>
      foldRight(ys, xs) { (z, zs) => Cons(z, zs) }
    }
  }

  /** Exercise 3.18 */
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight[A, List[B]](as, Nil) { (x, xs) =>
      Cons(f(x), xs)
    }

  /** Exercise 3.19 */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight[A, List[A]](as, Nil) { (x, xs) =>
      if(f(x)) Cons(x, xs) else xs
    }

  /** Exercise 3.20 */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  /** Exercise 3.23 */
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case _ => Nil
  }
}
