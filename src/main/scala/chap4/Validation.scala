package chap4

trait Validation[+E, +A] {
  import Validation._
  def orElse[EE >: E, B >: A](f: List[E] => Validation[EE, B]): Validation[EE, B] = this match {
    case Failure(v) => f(v)
    case Success(a) => this
  }
  def toEither: Either[E, A]
}

object Validation {
  case class Failure[+E](value: List[E]) extends Validation[E, Nothing] {
    override def toEither: Either[E, Nothing] = Left(value.head)
  }

  case class Success[+A](value: A) extends Validation[Nothing, A] {
    override def toEither: Either[Nothing, A] = Right(value)
  }
}
