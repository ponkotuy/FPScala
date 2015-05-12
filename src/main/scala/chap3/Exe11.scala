package chap3

object Exe11 extends App {
  assert(List.sum(List(1, 2, 3)) == 6)
  assert(List.sum(Nil) == 0)

  assert(List.product(List(1, 2, 3)) == 6.0)
  assert(List.product(Nil) == 1.0)

  assert(List.length(List(1, 2, 3)) == 3)
  assert(List.length(Nil) == 0)
}
