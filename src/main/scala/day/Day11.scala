package day

import run.{DayRunner, Result}
import utility.*

import scala.collection.{immutable, mutable}

class Day11 extends IDay(11) {
  override def execute(input: String): Result = {
    val nodeMap = NodeMap.read(input)
    Result(part1(nodeMap), part2(nodeMap))
  }

  private def part1(nodeMap: NodeMap) = {
    val cache = Cache[String, Long]

    def recurse(node: String): Long = cache.getOrCompute(node, {
      if (node == "out") {
        1
      } else {
        nodeMap.connectionsFor(node).toList.map(recurse).sum
      }
    })

    recurse("you")
  }

  private def part2(nodeMap: NodeMap) = {
    case class State(node: String, visitedDac: Boolean, visitedFft: Boolean) {
      def updateToNode(newNode: String): State = {
        val newVisitedDac = visitedDac || newNode == "dac"
        val newVisitedFft = visitedFft || newNode == "fft"
        State(newNode, newVisitedDac, newVisitedFft)
      }
    }

    val cache = Cache[State, Long]

    def recurse(state: State): Long = cache.getOrCompute(state, {
      if (state.node == "out") {
        (state.visitedDac && state.visitedFft).toInt
      } else {
        nodeMap.connectionsFor(state.node).toList.map(state.updateToNode).map(recurse).sum
      }
    })

    recurse(State("svr", false, false))
  }

  private case class NodeMap(nodes: immutable.Map[String, Set[String]]) {
    def connectionsFor(node: String): Set[String] = nodes(node)
  }

  private object NodeMap {
    def read(s: String): NodeMap = {
      val nodes = mutable.Map[String, Set[String]]()

      Helper.forEachLine(s, line => {
        val (name, connections) = line.splitPair(": ")
        val connSet = connections.split(" ").filterNot(_.isBlank).toSet
        nodes.put(name, connSet)
      })

      NodeMap(nodes.toMap)
    }
  }

  val test = Result.Test(5, None, "aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out")
}

object Day11 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day11()).run(skipTest = true)
  }
}
