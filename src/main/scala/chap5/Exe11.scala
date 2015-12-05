package chap5

object Exe11 extends App {
  assert(Stream.unfold(0) { _ => None }.toList == Nil)
  assert(Stream.unfold(0) { n => if(n < 10) Some((n + 1, n + 1)) else None }.toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
}
