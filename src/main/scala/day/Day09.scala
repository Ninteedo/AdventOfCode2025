package day

import run.{DayRunner, Result}
import utility.*

import scala.math.{max, min}

class Day09 extends IDay(9) {
  override def execute(input: String): Result = {
    val redTiles = Helper.readLines(input, Point2D.readCsv).toList
    val pairsWithAreas = redTiles.toIndexedSeq.combinationPairs.map((a, b) => ((a, b), a.areaWith(b))).toList.sortBy(_._2).reverse
    Result(part1(pairsWithAreas), part2(redTiles, pairsWithAreas))
  }

  private def part1(pairsWithAreas: List[((Point2D, Point2D), Long)]): Long = {
    pairsWithAreas.head._2
  }

  private def part2(redTiles: List[Point2D], pairsWithAreas: List[((Point2D, Point2D), Long)]): Long = {
    val edges = (redTiles.last :: redTiles).slidingPairs.toList
    val greenTiles = edges.flatMap((end1, end2) => {
      if (end1.x == end2.x) {
        val x = end1.x
        for (y <- Range.inclusive(min(end1.y, end2.y), max(end1.y, end2.y))) yield Point2D(x, y)
      } else {
        assert(end1.y == end2.y)
        val y = end1.y
        for (x <- Range.inclusive(min(end1.x, end2.x), max(end1.x, end2.x))) yield Point2D(x, y)
      }
    }).toSet
    val redTilesSet = redTiles.toSet
    val horizEdges = edges.filter((a, b) => a.x != b.x)
    val vertEdges = edges.filter((a, b) => a.y != b.y)

    def isInterior(point: Point2D): Boolean = {
      redTilesSet.contains(point) || greenTiles.contains(point) || {
        val xEdges = horizEdges.filter((a, b) => point.x.inRangeExclusive(min(a.x, b.x), max(a.x, b.x)))
        xEdges.count((a, b) => a.y >= point.y).isOdd && xEdges.count((a, b) => a.y <= point.y).isOdd && {
          val yEdges = vertEdges.filter((a, b) => point.y.inRangeExclusive(min(a.y, b.y), max(a.y, b.y)))
          yEdges.count((a, b) => a.x >= point.x).isOdd && yEdges.count((a, b) => a.x <= point.x).isOdd
        }
      }
    }

    def testEdge(edge: (Point2D, Point2D)): Boolean = {
      val (a, b) = edge
      if (a.x == b.x) {
        horizEdges.filter((c, d) => between(c.y, a.y, b.y) && betweenInclusive(a.x, c.x, d.x))
          .forall((c, d) => isInterior(Point2D(a.x, c.y - 1)) && isInterior(Point2D(a.x, c.y + 1)))
      } else {
        vertEdges.filter((c, d) => between(c.x, a.x, b.x) && betweenInclusive(a.y, c.y, d.y))
          .forall((c, d) => isInterior(Point2D(c.x - 1, a.y)) && isInterior(Point2D(c.x + 1, a.y)))
      }
    }

    def allGreen(corners: (Point2D, Point2D)): Boolean = {
      val (aa, bb) = corners
      val (ab, ba) = (Point2D(aa.x, bb.y), Point2D(bb.x, aa.y))
      List(aa, ab, bb, ba).forall(isInterior) && List((aa, ab), (ab, bb), (bb, ba), (ba, aa)).forall(testEdge)
    }

    pairsWithAreas.find((corners, area) => allGreen(corners)).map(_._2).get
  }

  private def orderedPair(a: Long, b: Long): (Long, Long) = (min(a, b), max(a, b))

  private def between(value: Long, first: Long, second: Long): Boolean = {
    val (l, r) = orderedPair(first, second)
    l < value && value < r
  }

  private def betweenInclusive(value: Long, first: Long, second: Long): Boolean = {
    val (l, r) = orderedPair(first, second)
    l <= value && value <= r
  }

  val test = List(
    Result.Test(50, Some(24), "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"),
    Result.Test(72, Some(40), "1,1\n5,1\n5,6\n7,6\n7,1\n9,1\n9,8\n1,8"),
  )
}

object Day09 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day09()).run()
  }
}
