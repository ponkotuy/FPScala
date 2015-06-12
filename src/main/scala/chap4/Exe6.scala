package chap4

object Exe6 extends App {
  val e = new Exception("Exe6")
  def left[A]: Either[Exception, A] = Left(e)
  assert(Right(1).map(_ + 1) == Right(2))
  assert(left[Int].map(_ + 1) == Left(e))
  // 面倒くさくなったのでテスト略
}
