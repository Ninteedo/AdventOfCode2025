package day

import run.{DayRunner, Result}
import utility.Direction.{East, West}
import utility.*

import scala.collection.mutable

class Day07 extends IDay(7) {
  override def execute(input: String): Result = {
    val grid = Grid2D.from2DCharArray(input, GridEntry.read)
    val startPos = grid.indicesWhere(_ == GridEntry.START).head
    val splitters = grid.indicesWhere(_ == GridEntry.SPLITTER)
    Result(part1(startPos, splitters), part2(startPos, splitters))
  }

  private def part1(startPos: Point2D, splitters: List[Point2D]) = {
    val usedSplitters = mutable.Set.empty[Point2D]

    def recurse(beamStartPos: Point2D): Long = findSplitter(beamStartPos, splitters) match {
      case Some(splitter) =>
        if (usedSplitters.add(splitter)) 1 + recurse(splitter + East) + recurse(splitter + West)
        else 0
      case None => 0
    }

    recurse(startPos)
  }

  private def part2(startPos: Point2D, splitters: List[Point2D]) = {
    val memoCache = Cache[Point2D, Long]

    def recurse(beamStartPos: Point2D): Long = memoCache.getOrCompute(beamStartPos, {
      findSplitter(beamStartPos, splitters) match {
        case Some(splitter) => recurse(splitter + East) + recurse(splitter + West)
        case None => 1
      }
    })

    recurse(startPos)
  }

  private def findSplitter(beamStartPos: Point2D, splitters: List[Point2D]): Option[Point2D] = {
    splitters.find(splitter => splitter.x == beamStartPos.x && splitter.y > beamStartPos.y)
  }

  private enum GridEntry {
    case START, SPLITTER, EMPTY
  }

  private object GridEntry {
    def read(c: Char): GridEntry = c match {
      case 'S' => START
      case '^' => SPLITTER
      case '.' => EMPTY
      case _   => throw new IllegalArgumentException(s"Character '$c' is not a valid grid entry")
    }
  }

  val test = Result.Test(".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............", 21, Some(40))
}

object Day07 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day07()).run()
  }
}
