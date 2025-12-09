package day

import run.{DayRunner, Result}
import utility.*

import scala.collection.mutable

class Day08 extends IDay(8) {
  override def execute(input: String): Result = {
    val junctionBoxPositions = Helper.readLines(input, Point3D.readCsv).toList
    val connections = junctionBoxPositions.toIndexedSeq.combinationPairs.toList.sortBy(pair => pair._1.euclidDist(pair._2))
    Result(part1(junctionBoxPositions, connections), part2(junctionBoxPositions, connections))
  }

  private def part1(junctionBoxPositions: List[Point3D], connections: List[(Point3D, Point3D)]) = {
    val uf = UnionFind(junctionBoxPositions)
    connections.take(1000).foreach((a, b) => uf.unite(a, b))
    uf.componentSizes.toList.sorted.reverse.take(3).product
  }

  private def part2(junctionBoxPositions: List[Point3D], connections: List[(Point3D, Point3D)]) = {
    val uf = UnionFind(junctionBoxPositions)
    val n = junctionBoxPositions.size

    val conn = connections.find((a, b) => uf.unite(a, b) == n).get
    conn._1.x * conn._2.x
  }

  private class UnionFind[A](private val xs: Iterable[A]) {
    private val parent = mutable.Map.from(xs.map(x => x -> x))
    private val size   = mutable.Map.from(xs.map(x => x -> 1))

    private def find(x: A): A = {
      val p = parent(x)
      if (p == x) x
      else {
        val root = find(p)
        parent.update(x, root)
        root
      }
    }

    def unite(a: A, b: A): Int = {
      val (ra, rb) = (find(a), find(b))
      if (ra == rb) size(ra)
      else {
        val (big, small) = if (size(ra) >= size(rb)) (ra, rb) else (rb, ra)
        parent(small) = big
        size(big) += size(small)
        size(big)
      }
    }

    def componentSizes: Iterable[Int] = size.values

    def largestSize: Int = size.values.max
  }

  val test = Result.Test(40, Some(25272), "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689")
}

object Day08 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day08()).run(skipTest = true)
  }
}
