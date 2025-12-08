package day

import run.{DayRunner, Result}
import utility.*

import scala.collection.mutable

class Day08 extends IDay(8) {
  override def execute(input: String): Result = {
    val junctionBoxPositions = Helper.readLines(input, Point3D.readCsv).toList
    Result(part1(junctionBoxPositions), part2(junctionBoxPositions))
  }

  private def part1(junctionBoxPositions: List[Point3D]) = {
    val pairs = junctionBoxPositions.combinationPairs.toList
    val connections = pairs.sortBy(pair => pair._1.euclidDist(pair._2)).take(1000)
    val circuits = mutable.Buffer[Set[Point3D]]()
    connections.foreach(conn => {
      val matches = circuits.zipWithIndex.filter(p => p._1.contains(conn._1) || p._1.contains(conn._2))
      if (matches.isEmpty) {
        circuits.append(Set(conn._1, conn._2))
      } else if (matches.size == 1) {
        matches.foreach(p => circuits.update(p._2, p._1 + conn._1 + conn._2))
      } else {
        val combinedCircuit = matches.map(_._1).foldLeft(Set.empty[Point3D])((a, b) => a.union(b))
        matches.foreach(p => circuits.update(p._2, Set.empty))
        circuits.update(matches.head._2, combinedCircuit)
      }
    })
    circuits.toList.map(_.size).sorted.reverse.take(3).product
  }

  private def part2(junctionBoxPositions: List[Point3D]) = {
    val pairs = junctionBoxPositions.combinationPairs.toList
    val connections = pairs.sortBy(pair => pair._1.euclidDist(pair._2)).toVector
    val circuits = mutable.Buffer[Set[Point3D]]()

    var completedConnection: Option[(Point3D, Point3D)] = None
    var i = 0
    var maxCircuitSize = 0
    while (i < connections.size && completedConnection.isEmpty) {
      val conn = connections(i)
      i += 1
      val matches = circuits.zipWithIndex.filter(p => p._1.contains(conn._1) || p._1.contains(conn._2))
      if (matches.isEmpty) {
        circuits.append(Set(conn._1, conn._2))
        maxCircuitSize = math.max(maxCircuitSize, 2)
      } else if (matches.size == 1) {
        matches.foreach(p => circuits.update(p._2, p._1 + conn._1 + conn._2))
        maxCircuitSize = math.max(maxCircuitSize, circuits(matches.head._2).size)
      } else {
        val combinedCircuit = matches.map(_._1).foldLeft(Set.empty[Point3D])((a, b) => a.union(b))
        matches.foreach(p => circuits.update(p._2, Set.empty))
        circuits.update(matches.head._2, combinedCircuit)
        maxCircuitSize = circuits.map(_.size).max
      }
      if (maxCircuitSize == junctionBoxPositions.size) {
        completedConnection = Some(conn)
      }
    }
    completedConnection.get._1.x * completedConnection.get._2.x
  }

  val test = Result.Test(40, Some(25272), "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689")
}

object Day08 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day08()).run(skipTest = true)
  }
}
