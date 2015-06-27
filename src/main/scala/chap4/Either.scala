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

  def isLeft: Boolean
  def isRight: Boolean = !isLeft

  def left: E = this match {
    case Left(v) => v
    case Right(_) => throw new RuntimeException(s"Not supported: ${this}.left")
  }

  def right: A = this match {
    case Left(_) => throw new RuntimeException(s"Not supported: ${this}.right")
    case Right(v) => v
  }
}
case class Left[+E](value: E) extends Either[E, Nothing] {
  override def isLeft: Boolean = true
}
case class Right[+A](value: A) extends Either[Nothing, A] {
  override def isLeft: Boolean = false
}

object Either {
  def Try[A](a: => A): Either[Throwable, A] =
    try { Right(a) } catch { case e: Throwable => Left(e) }
}
