package chap5

object Exe12 extends App {
  val ones = Stream.unfold(Unit) { _ => Some(1, Unit) }
  def constant[A](a: A) = Stream.unfold(Unit) { _ => Some(a, Unit) }
  def from(n: Int) = Stream.unfold(n) { x => Some(x, x + 1) }

  val fibs = Stream.unfold((0, 1)) { case (x, y) =>
    Some(x, (y, x + y))
  }

  assert(ones.take(3).toList == Stream.ones.take(3).toList)
  assert(constant(9).take(3).toList == Stream.constant(9).take(3).toList)
  assert(from(4).take(3).toList == Stream.from(4).take(3).toList)
  assert(fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
}
