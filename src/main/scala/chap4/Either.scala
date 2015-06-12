package chap4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](f: E => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => f(e)
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) = (this, b) match {
    case (Right(_a), Right(_b)) => Right(f(_a, _b))
    case (Left(_a), _) => Left(_a)
    case (_, Left(_b)) => Left(_b)
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Throwable, A] =
    try { Right(a) } catch { case e: Throwable => Left(e) }
}
