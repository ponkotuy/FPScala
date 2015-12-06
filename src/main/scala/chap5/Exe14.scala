package chap5

object Exe14 extends App {
  assert(Stream.from(1).startsWith(Stream(1, 2)))
  assert(!Stream.from(1).startsWith(Stream(1, 2, 4)))
}
