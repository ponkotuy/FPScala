package chap4

object Exe1 extends App {
  assert(Some(1).map(_ + 1) == Some(2))
  assert((None: Option[Int]).map(_ + 1) == None)
  assert(Some(1).flatMap(x => if(x > 0) Some(x - 1) else None) == Some(0))
  assert(Some(0).flatMap(x => if(x > 0) Some(x - 1) else None) == None)
  assert((None: Option[Int]).flatMap(x => if(x > 0) Some(x - 1) else None) == None)
  assert(Some(1).getOrElse(0) == 1)
  assert(None.getOrElse(0) == 0)
  assert(Some(1).orElse(Some(2)) == Some(1))
  assert(None.orElse(Some(2)) == Some(2))
  assert(Some(1).filter(_ > 0) == Some(1))
  assert(Some(0).filter(_ > 0) == None)
  assert((None: Option[Int]).filter(_ > 0) == None)
}
