package chap3

import scala.util.Try

object Exe2 extends App {
  assert(List.tail(List(1, 2, 3)) == List(2, 3))
  assert(Try { List.tail(Nil) }.isFailure)
}
