package chap5

object Exe13 extends App {
  def map[A, B](xs: Stream[A])(f: A => B): Stream[B] = Stream.unfold(xs) {
    case Cons(y, ys) => Some(f(y()), ys())
    case Empty => None
  }

  def take[A](xs: Stream[A])(n: Int): Stream[A] = Stream.unfold((xs, n)) {
    case (Cons(y, ys), 0) => None
    case (Cons(y, ys), m) => Some(y(), (ys(), m - 1))
    case (Empty, _) => None
  }

  def takeWhile[A](xs: Stream[A])(f: A => Boolean): Stream[A] = Stream.unfold(xs) {
    case Cons(y, ys) if f(y()) => Some(y(), ys())
    case Cons(_, _) => None
    case Empty => None
  }

  def zipAll[A, B](xs: Stream[A], ys: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((xs, ys)) {
    case (Cons(a, as), Cons(b, bs)) => Some((Some(a()), Some(b())), (as(), bs()))
    case (Cons(a, as), Empty) => Some((Some(a()), None), (as(), Empty))
    case (Empty, Cons(b, bs)) => Some((None, Some(b())), (Empty, bs()))
    case (Empty, Empty) => None
  }

  assert(map(Stream.from(1))(_ + 1).take(5).toList == List(2, 3, 4, 5, 6))
  assert(take(Stream.from(1))(3).toList == List(1, 2, 3))
  assert(take(Stream(1, 2, 3))(5).toList == List(1, 2, 3))
  assert(takeWhile(Stream.from(1))(_ < 5).toList == List(1, 2, 3, 4))
  assert(zipAll(Stream(1, 2), Stream(2, 3, 4)).toList == List((Some(1), Some(2)), (Some(2), Some(3)), (None, Some(4))))
}
