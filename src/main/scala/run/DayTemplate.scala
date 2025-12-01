package run

import utility.IDay

class DayTemplate extends IDay(-1) {
  override def execute(input: String): Result = {
    Result(part1(), part2())
  }

  private def part1() = {
    incomplete
  }

  private def part2() = {
    incomplete
  }

  val test = Result.Test(???, ???, None)
}

object DayTemplate {
  def main(args: Array[String]): Unit = {
    DayRunner(DayTemplate()).run()
  }
}
