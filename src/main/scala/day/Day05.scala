package day

import run.{DayRunner, Result}
import utility.*

class Day05 extends IDay(5) {
  override def execute(input: String): Result = {
    val (rangesString, ingredientsString) = input.splitPair("\n\n")
    val freshRanges = Helper.readLines(rangesString, FreshRange.read).toList
    val ingredients = Helper.readLines(ingredientsString, _.toLong).toList
    Result(part1(freshRanges, ingredients), part2(freshRanges))
  }

  private def part1(freshRanges: List[FreshRange], ingredients: List[Long]): Long = {
    ingredients.count(ingredient => freshRanges.exists(_.includes(ingredient)))
  }

  private def part2(freshRanges: List[FreshRange]): Long = {
    freshRanges
      .sortBy(_.start)
      .foldLeft(List.empty[FreshRange])((acc, next) => acc match {
        case head :: tail if next.start <= head.end + 1 => FreshRange(head.start, math.max(head.end, next.end)) :: tail
        case _ => next :: acc
      })
      .map(_.size)
      .sum
  }

  private case class FreshRange(start: Long, end: Long) {
    def includes(ingredient: Long): Boolean = ingredient.inRangeInclusive(start, end)

    val size: Long = end - start + 1
  }

  private object FreshRange {
    def read(line: String): FreshRange = {
      val (l, r) = line.splitPair("-")
      FreshRange(l.toLong, r.toLong)
    }
  }

  val test = Result.Test("3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32", 3, Some(14))
}

object Day05 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day05()).run()
  }
}
