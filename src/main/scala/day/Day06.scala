package day

import run.{DayRunner, Result}
import utility.*

class Day06 extends IDay(6) {
  override def execute(input: String): Result = {
    Result(part1(input), part2(input))
  }

  private def part1(input: String): Long = {
    val rows = Helper.readLines(input, _.split("\\s+").toVector.filter(!_.isBlank)).toVector
    val cols = rows.transpose

    cols.map(col => applyOperation(col.last.head, col.init.map(_.toLong))).sum
  }

  private def part2(input: String) = {
    def digitsToNum(chars: Iterable[Char]): Long = String.copyValueOf(chars.toArray).strip.toLong

    val charGrid = Grid2D.from2DCharArray(input, identity)
    charGrid
      .lastRow
      .zipWithIndex
      .filter(_._1 != ' ')
      .appended((' ', charGrid.colCount + 1))       // add extra entry for end-of-column range (for last operation)
      .sliding(2)
      .map(v => (v(0)._1, (v(0)._2, v(1)._2 - 2)))
      .map((c, indices) =>                          // (opChar, (start column index, end column index))
        (c, (indices._1 to indices._2).map(col => digitsToNum(charGrid.getCol(col).init)))  // read column as long
      )
      .map(group => applyOperation(group._1, group._2))
      .sum
  }

  private def applyOperation(opChar: Char, nums: Iterable[Long]): Long = opChar match {
    case '+' => nums.sum
    case '*' => nums.product
  }

  val test = Result.Test(4277556, Some(3263827), "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ")
}

object Day06 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day06()).run()
  }
}
