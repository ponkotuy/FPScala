package chap5

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /** Exe1 */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /** Exe2 */
  def take(n: Int): Stream[A] =
    if(n <= 0) Empty else {
      this match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => t().take(n - 1))
      }
    }

  /** Exe2 */
  def drop(n: Int): Stream[A] =
    if(n <= 0) this else {
      this match {
        case Empty => Empty
        case Cons(h, t) => t().drop(n - 1)
      }
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
