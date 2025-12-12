package day

import run.{DayRunner, Result}
import utility.*

class Day12 extends IDay(12) {
  override def execute(input: String): Result = {
    val splits = input.split("\n\n")
    val shapes = splits.init.map(Shape.read).toVector
    val regions = Helper.readLines(splits.last, Region.read)
    Result(part1(shapes, regions), part2())
  }

  private def part1(shapes: Vector[Shape], regions: Iterable[Region]) = {
    regions.count(region => {
      region.area >= region.requires.zip(shapes).map((n, shape) => n * shape.size).sum
    })
  }

  private def part2() = {
    "Merry Christmas!"
  }

  private case class Shape(occupies: Set[Point2D]) {
    val size: Int = occupies.size
  }

  private object Shape {
    def read(s: String): Shape = {
      val noHeader = s.drop(s.indexOf('\n') + 1)
      Shape(Grid2D.from2DCharArray(noHeader, identity).indicesWhere(_ == '#').toSet)
    }
  }

  private case class Region(width: Int, height: Int, requires: Vector[Int]) {
    val area: Int = width * height

    val requiresSimpleArea: Int = requires.sum * 9
  }

  private object Region {
    def read(s: String): Region = {
      val (dimensionsString, entriesString) = s.splitPair(": ")
      val (widthString, heightString) = dimensionsString.splitPair("x")
      val entries = entriesString.split(" ").map(_.toInt)
      Region(widthString.toInt, heightString.toInt, entries.toVector)
    }
  }

  val test = Result.Test(2, None, "0:\n###\n##.\n##.\n\n1:\n###\n##.\n.##\n\n2:\n.##\n###\n##.\n\n3:\n##.\n###\n##.\n\n4:\n###\n#..\n###\n\n5:\n###\n.#.\n###\n\n4x4: 0 0 0 0 2 0\n12x5: 1 0 1 0 2 2\n12x5: 1 0 1 0 3 2")
}

object Day12 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day12()).run(skipTest = true)
  }
}
