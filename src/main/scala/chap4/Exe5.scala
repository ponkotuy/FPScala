package chap4

import scala.util.{Success, Try}

object Exe5 extends App {
  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] = {
    xs.foldRight(Some(Nil): Option[List[B]]) { (x, opts) =>
      Exe3.map2(f(x), opts) { (y, ys) => y :: ys }
    }
  }

  def toInt(str: String): Option[Int] = Try { str.toInt } match {
    case Success(x) => Some(x)
    case _ => None
  }

  assert(traverse(List("1", "2", "3"))(toInt) == Some(List(1, 2, 3)))
  assert(traverse(List("1", "a"))(toInt) == None)
}
