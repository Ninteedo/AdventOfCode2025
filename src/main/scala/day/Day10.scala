package day

import run.{DayRunner, Result}
import utility.*

import scala.collection.mutable

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

  private def part2(machines: Iterable[Machine]): Int = {
//    def minimumRequiredButtonPresses(machine: Machine): Int = {
//      printAndReturn(P2SearchNode(machine, LazyList.continually(0).take(machine.joltageRequirements.length).toVector, List.empty, 0, None).bestFirstSearch().get.getResult)
//    }
    def minimumRequiredButtonPresses(machine: Machine): Int = {
      val buttonMaxes: IndexedSeq[Int] = machine.buttons.map(_.map(machine.joltageRequirements).min)
      val buttonLinks: IndexedSeq[Set[Int]] = machine.buttons.map(_.flatMap(machine.buttonsForLight)).zipWithIndex.map((linked, button) => linked - button)

      var runningBest = Int.MaxValue
      var checked = 0
      val buttonCount = machine.buttons.size

      def recurse(button: Int, curr: IndexedSeq[Int]): Unit = {
        checked += 1
        if (curr.sum >= runningBest) {
//          println(s"Pruning $curr (${curr.sum})")
          return
        }

        val remainingJoltage = machine.remainingJoltage(curr)
        if (remainingJoltage.exists(_ < 0)) {
//          println(s"Excluding invalid joltages $curr")
          return
        } else if (button >= buttonCount) {
          if (remainingJoltage.forall(_ == 0)) {
            runningBest = math.min(runningBest, curr.sum)
            println(s"$runningBest $curr")
          } else {
//            println(s"$curr did not add up ($remainingJoltage)")
          }
        } else {
          val links = buttonLinks(button)
          if (links.isEmpty) {
            recurse(button + 1, curr :+ buttonMaxes(button))
          } else {
            val maxAllowed = machine.buttons(button).map(remainingJoltage).min
            val definedLinks = links.filter(_ < button)
            val remainingLinks = links.filter(_ > button)
            if (remainingLinks.isEmpty) {
              recurse(button + 1, curr :+ maxAllowed)
            } else {
//          println(s"Recursing from $curr, adding up to $maxAllowed")
              Range.inclusive(0, maxAllowed).foreach(n => recurse(button + 1, curr :+ n))
            }
          }
        }
      }

      recurse(0, IndexedSeq.empty)
      printAndReturn(runningBest)
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

  private case class P2SearchNode(machine: Machine,
                                  joltages: Vector[Int],
                                  pressedButtons: List[Int],
                                  currentButton: Int,
                                  parent: Option[P2SearchNode]) extends SearchNode[P2SearchNode] {

//    override lazy val calculateOrderingValue: Int = machine.joltageRequirements.zip(joltages).map((required, current) => required - current).sum
    override lazy val calculateOrderingValue: Int = -pressedButtons.size

    override def descendents: Iterable[P2SearchNode] = {
      Range(currentButton, machine.buttons.length).map(i => (addToggles(machine.buttons(i)), i)).filter(_._1.isDefined)
        .map((joltages, i) => P2SearchNode(machine, joltages.get, i :: pressedButtons, i, parent))
//      machine.buttons.zipWithIndex
//        .flatMap((toggles, i) => addToggles(toggles)
//          .map(joltages => P2SearchNode(machine, joltages, i :: pressedButtons, parent)))
    }

    override lazy val atGoal: Boolean = joltages == machine.joltageRequirements
    override lazy val getParent: Option[P2SearchNode] = parent
    override lazy val getResult: Int = pressedButtons.length

//    override val filterDuplicates: Boolean = true
//
//    override def isDuplicateOf(other: SearchNode[P2SearchNode]): Boolean = {
//      val o = other.asInstanceOf[P2SearchNode]
//      machine == o.machine && joltages == o.joltages && pressedButtons.groupBy(identity) == o.pressedButtons.groupBy(identity)
//    }

    private def addToggles(toggles: Set[Int]): Option[Vector[Int]] = {
      val res = toggles.foldLeft(joltages)((joltages, toggle) => joltages.updated(toggle, joltages(toggle) + 1))
      if (res.zipWithIndex.forall((joltage, i) => joltage <= machine.joltageRequirements(i))) Some(res)
      else None
    }
  }

  val test = Result.Test(7, Some(33), "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")
}

object Day10 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day10()).run()
  }
}
