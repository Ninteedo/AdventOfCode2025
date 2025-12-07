package day

import run.{DayRunner, Result}
import utility.*

import utility.IDay

class Day01 extends IDay(1) {
  override def execute(input: String): Result = {
    val moves = Helper.readLines(input, Move.fromString)
    Result(part1(moves), part2(moves))
  }

  private def part1(moves: Iterable[Move]) = {
    def applyMove(state: State, move: Move): State = {
      val pos = absMod(state.pos + move.dir * move.count, 100)
      val zeroes = state.zeroes + (pos == 0).toInt
      State(pos, zeroes)
    }

    moves.foldLeft(START)(applyMove).zeroes
  }

  private def part2(moves: Iterable[Move]) = {
    def applyMove(state: State, move: Move): State = {
      val change = move.dir * move.count
      val posNoMod = state.pos + change
      val pos = absMod(posNoMod, 100)
      val zeroPasses = (state.pos != 0 && posNoMod <= 0).toInt + Math.abs(posNoMod / 100)
      State(pos, state.zeroes + zeroPasses)
    }

    moves.foldLeft(START)(applyMove).zeroes
  }

  private case class State(pos: Int, zeroes: Int)

  private val START: State = State(50, 0)

  private case class Move(dir: Int, count: Int)

  private object Move {
    def fromString(s: String): Move = {
      val dir = s.head match {
        case 'L' => -1
        case 'R' => 1
        case c => throw new IllegalArgumentException(s"Move direction can only be 'L' or 'R', not '$c'")
      }
      val count = s.tail.toInt
      Move(dir, count)
    }
  }

  private def absMod(n: Int, mod: Int): Int = {
    val res = n % mod
    res + ((res < 0).toInt * mod)
  }

  val test = Result.Test(3, Some(6), "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82")
}

object Day01 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day01()).run()
  }
}
