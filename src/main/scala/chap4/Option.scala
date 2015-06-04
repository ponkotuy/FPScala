package chap4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this.flatMap(x => if(f(x)) Some(x) else None)

  def isDefined = this match {
    case Some(_) => true
    case None => false
  }

  def isEmpty = !isDefined
  def nonEmpty = isDefined

  def foreach[U](f: A => U): Unit = map(f)
}
case class Some[+A](getUnsafe: A) extends Option[A]
case object None extends Option[Nothing]
