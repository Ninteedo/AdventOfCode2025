package day

import run.{DayRunner, Result}
import utility.*

import scala.collection.mutable
import com.google.ortools.Loader
import com.google.ortools.sat.*

class Day10 extends IDay(10) {
  override def execute(input: String): Result = {
    val machines = Helper.readLines(input, Machine.read)
    Result(part1(machines), part2(machines))
  }

  private def part1(machines: Iterable[Machine]): Int = {
    def minimumRequiredButtonPresses(machine: Machine): Int = {
      P1SearchNode(machine, Set.empty, List.empty).breadthFirstSearch.get.pressedButtons.size
    }

    machines.map(minimumRequiredButtonPresses).sum
  }

  private def part2(machines: Iterable[Machine]) = {
    def minimumRequiredButtonPresses(machine: Machine): Long = {
      Loader.loadNativeLibraries()

      val model = new CpModel()

      val b = machine.buttons.size
      val n = machine.joltageRequirements.size

      val maxPress = machine.joltageRequirements.max
      val pressArgs: Array[LinearArgument] = Array.tabulate(b)(j => model.newIntVar(0, maxPress, s"press_$j"))

      (0 until n).foreach(i => {
        val linkedButtons = machine.buttons.zipWithIndex.filter((linked, j) => linked.contains(i)).map(_._2)
        val sumExpr = LinearExpr.sum(linkedButtons.map(pressArgs).toArray)
        model.addEquality(sumExpr, machine.joltageRequirements(i))
      })

      model.minimize(LinearExpr.sum(pressArgs))

      val solver = new CpSolver()
      solver.solve(model)

      pressArgs.map(solver.value).sum
    }

    machines.map(minimumRequiredButtonPresses).sum
  }

  private case class Machine(indicatorLightLength: Int, requiredLights: Set[Int], buttons: Vector[Set[Int]], joltageRequirements: Vector[Int]) {
    val indicatorLightIndices: Seq[Int] = Range(0, indicatorLightLength)

    val buttonsForLight: Vector[List[Int]] = {
      indicatorLightIndices.map(i =>
        buttons.zipWithIndex
          .filter(_._1.contains(i))
          .map(_._2)
          .toList
        ).toVector
    }

    def testButtonPressesAgainstJoltageRequirements(presses: IndexedSeq[Int]): Boolean = {
      joltageRequirements.zipWithIndex.forall((joltage, i) => buttonsForLight(i).map(presses).sum == joltage)
    }

    def remainingJoltage(presses: IndexedSeq[Int]): IndexedSeq[Int] = {
      val n = presses.size
      joltageRequirements.zipWithIndex.map((joltage, i) => joltage - buttonsForLight(i).filter(_ < n).map(presses).sum)
    }
  }

  private object Machine {
    def read(line: String): Machine = {
      val splits = line.split(" ")
      val indicatorLightsString = splits.head.stripPrefix("[").stripSuffix("]")
      val indicatorLightLength = indicatorLightsString.length
      val requiredLights = indicatorLightsString.zipWithIndex.filter(_._1 == '#').map(_._2).toSet
      val buttons = splits.tail.init.toList
        .map(_.stripPrefix("(").stripSuffix(")"))
        .map(_.split(",").map(_.toInt).toSet)
        .toVector
      val joltageRequirements = splits.last.stripPrefix("{").stripSuffix("}").split(",").map(_.toInt).toVector
      Machine(indicatorLightLength, requiredLights, buttons, joltageRequirements)
    }
  }

  private case class P1SearchNode(machine: Machine, litLights: Set[Int], pressedButtons: List[Int]) {
    val atGoal: Boolean = litLights == machine.requiredLights

    def descendents: Iterable[P1SearchNode] = {
      machine.buttons
        .zipWithIndex
        .filter((toggledLights, i) => !pressedButtons.headOption.contains(i))
        .filter((toggledLights, i) => !toggledLights.forall(litLights.contains))
        .map((toggledLights, i) => P1SearchNode(machine, toggleLights(litLights, machine.buttons(i)), i :: pressedButtons))
    }

    def toggleLights(litLights: Set[Int], toggles: Set[Int]): Set[Int] = {
      toggles.foldLeft(litLights)((curr, toggle) => if (curr.contains(toggle)) curr - toggle else curr + toggle)
    }

    def breadthFirstSearch: Option[P1SearchNode] = {
      val frontier = mutable.Queue(this)

      while (frontier.nonEmpty) {
        val node: P1SearchNode = frontier.dequeue()
        if (node.atGoal) return Some(node)

        val nodesToAdd = node.descendents
        nodesToAdd.foreach(frontier.enqueue)
      }
      None
    }
  }

  val test = Result.Test(7, Some(33), "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")
}

object Day10 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day10()).run()
  }
}
