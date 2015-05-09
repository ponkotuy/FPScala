package chap2

object Exe1 extends App {
  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case x => fib(x - 2) + fib(x - 1)
  }

  assert(fib(0) == 0)
  assert(fib(1) == 1)
  assert(fib(10) == 55)
  assert(fib(20) == 6765)
}
