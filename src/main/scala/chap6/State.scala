package chap6

/**
  * State monad(Exe6.10)
  */
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { state =>
    val (a, state2) = run(state)
    (f(a), state2)
  }

  def foreach(f: A => Unit): State[S, Unit] = State { state =>
    val (a, state2) = run(state)
    f(a)
    (Unit, state2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { state =>
    val (a, state2) = run(state)
    f(a).run(state2)
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State { s => (a, s) }

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = State { state =>
    val (a, state2) = sa.run(state)
    val (b, state3) = sb.run(state2)
    (f(a, b), state3)
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State { state =>
    fs match {
      case x :: xs =>
        val (a, state2) = x.run(state)
        val (as, lastState) = sequence(xs).run(state2)
        (a :: as, lastState)
      case _ => (Nil, state)
    }
  }
}
