package chap3

import scala.util.Try

object Exe3 extends App {
  assert(List.setHead(List(1, 2, 3), 2) == List(2, 2, 3))
  assert(Try { List.setHead(Nil, 1) }.isFailure)
}
