package chap5

trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def head: A = this match {
    case Cons(h, _) => h()
    case _ => throw new NoSuchElementException
  }

  def tail: Stream[A] = this match {
    case Cons(_, t) => t()
    case _ => throw new NoSuchElementException
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

  /** Exe3 */
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if(f(h())) Cons(h, () => t().takeWhile(f)) else Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def exists(f: A => Boolean): Boolean = foldRight(false) { (a, b) => f(a) || b }

  /** Exe4 */
  def forAll(f: A => Boolean): Boolean = foldRight(true) { (a, b) => f(a) && b }

  /** Exe5 */
  def takeWhile2(f: A => Boolean): Stream[A] = foldRight[Stream[A]](empty) { (a, b) =>
    if(f(a)) cons(a, b) else empty
  }

  /** Exe6 */
  def headOption2: Option[A] = foldRight[Option[A]](None) { (a, _) => Some(a) }

  /** Exe7 */
  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](empty) { (a, b) => cons(f(a), b) }

  /** Exe7 */
  def filter(f: A => Boolean): Stream[A] = foldRight[Stream[A]](empty) { (a, b) =>
    if(f(a)) cons(a, b) else b
  }

  /** Exe7 */
  def append[B >: A](x: => B): Stream[B] = foldRight(cons(x, empty)) { (a, b) => cons(a, b) }

  /** Exe7 */
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](empty) { (a, b) =>
    f(a).foldRight(b) { (a, b) => cons(a, b) }
  }

  def zip[B](ys: Stream[B]): Stream[(A, B)] = Stream.zip(this, ys)

  /** Exe14 */
  def startsWith[B >: A](other: Stream[B]): Boolean = Stream.unfold((this, other)) {
    case (Cons(x, xs), Cons(y, ys)) if x() == y() => Some(true, (xs(), ys()))
    case (Cons(_, _), Cons(_, _)) => Some(false, (Empty, Empty))
    case (_, Empty) => None
  }.forAll(identity)

  /** Exe15 */
  def tails: Stream[Stream[A]] = cons(this, Stream.unfold(this) {
    case Cons(_, xs) => Some(xs(), xs())
    case Empty => None
  })
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

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def zip[A, B](xs: Stream[A], ys: Stream[B]): Stream[(A, B)] = xs match {
    case Empty => Empty
    case Cons(h, t) =>
      ys match {
        case Empty => Empty
        case Cons(h_, t_) =>
          Cons(() => (h(), h_()), () => zip(t(), t_()))
      }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
}
