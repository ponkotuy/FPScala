package chap6

object Exe11 extends App {
  import Input._

  def simulate(input: Input): State[Machine, (Int, Int)] = State { machine =>
    if(machine.candies > 0) {
      (input, machine.locked) match {
        case (Coin, true) => (machine.coins + 1, machine.candies) -> machine.inputCoin
        case (Turn, false) => (machine.coins, machine.candies - 1) -> machine.inputTurn
        case _ => (machine.coins, machine.candies) -> machine
      }
    } else (machine.coins, machine.candies) -> machine
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State.sequence(inputs.map(simulate)).map(_.last)
  }

  val machine = Machine(locked = true, 5, 10)
  simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
      .foreach { case (coins, candies) =>
        assert(coins == 14)
        assert(candies == 1)
      }.run(machine)
}

sealed abstract class Input
object Input {
  case object Coin extends Input
  case object Turn extends Input
}

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def inputCoin = copy(locked = false, coins = coins + 1)
  def inputTurn = copy(locked = true, candies = candies - 1)
}
