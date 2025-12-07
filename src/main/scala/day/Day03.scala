package day

import run.{DayRunner, Result}
import utility.*

import scala.annotation.tailrec

class Day03 extends IDay(3) {
  override def execute(input: String): Result = {
    val banks = Helper.readLines(input, BatteryBank.read).toList
    Result(part1(banks), part2(banks))
  }

  private def part1(banks: List[BatteryBank]) = {
    banks.map(_.maxJolts(2)).sum
  }

  private def part2(banks: List[BatteryBank]) = {
    banks.map(_.maxJolts(12)).sum
  }

  private case class BatteryBank(batteries: List[Int]) {
    private val n: Int = batteries.size

    @tailrec
    final def maxJolts(batteryCount: Int, offset: Int = 0, acc: Long = 0): Long = {
      val (digit, i) = batteries.zipWithIndex
        .filter(_._2.inRangeInclusive(offset, n - batteryCount))
        .maxBy(_._1)
      val newAcc = acc * 10 + digit
      if (batteryCount == 1) newAcc else maxJolts(batteryCount - 1, i + 1, newAcc)
    }
  }

  private object BatteryBank {
    def read(line: String): BatteryBank = {
      BatteryBank(line.map(c => c.toString.toInt).toList)
    }
  }

  val test = Result.Test(357, Some(3121910778619L), "987654321111111\n811111111111119\n234234234234278\n818181911112111")
}

object Day03 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day03()).run()
  }
}
