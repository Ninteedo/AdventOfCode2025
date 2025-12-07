package day

import run.{DayRunner, Result}
import utility.*

import scala.annotation.tailrec

class Day04 extends IDay(4) {
  override def execute(input: String): Result = {
    val rollsGrid = Grid2D.from2DCharArray(input, _ == '@')
    val indexedRollsGrid = rollsGrid.zipWithIndex
    Result(part1(indexedRollsGrid), part2(indexedRollsGrid))
  }

  private def part1(rollsGrid: Grid2D[(Boolean, Point2D)]) = {
    rollsGrid.count((isRoll, point) => isRoll && isForkliftAccessible(rollsGrid, point))
  }

  private def part2(rollsGrid: Grid2D[(Boolean, Point2D)]) = {
    @tailrec
    def removeAccessibleRolls(original: Grid2D[(Boolean, Point2D)]): Grid2D[(Boolean, Point2D)] = {
      val updated = original.map((isRoll, point) => (isRoll && !isForkliftAccessible(original, point), point))
      if (updated == original) updated
      else removeAccessibleRolls(updated)
    }

    val originalCount = rollsGrid.count(_._1)
    val updatedCount = removeAccessibleRolls(rollsGrid).count(_._1)
    originalCount - updatedCount
  }

  private def isForkliftAccessible(rollsGrid: Grid2D[(Boolean, Point2D)], point: Point2D): Boolean = {
    point.adjacents.count(p => rollsGrid.atOption(p).exists(_._1)) < 4
  }

  val test = Result.Test(13, Some(43), "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.")
}

object Day04 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day04()).run()
  }
}
