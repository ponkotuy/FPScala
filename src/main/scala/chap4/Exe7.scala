package chap4

object Exe7 extends App {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldLeft(Right(Nil): Either[E, List[A]]) { (x, y) =>
      for {
        _x <- x
        _y <- y
      } yield _y :: _x
    }.map(_.reverse)
  }

  def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sequence(xs.map(f))

  def toInt(str: String): Either[Throwable, Int] = Either.Try(str.toInt)

  assert(sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
  assert(sequence(List(Right(1), Left("Error"), Left("NonError"))) == Left("Error"))
  assert(traverse(List("1", "2"))(toInt) == Right(List(1, 2)))
  val left = traverse(List("1", "a"))(toInt)
  assert(left.isLeft)
  assert(left.left.isInstanceOf[NumberFormatException])
}
