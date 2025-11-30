package run

import utility.{IDay, StopWatch}

import scala.io.Source

case class DayRunner(day: IDay) {
  private val n = day.dayNumber
  private val dayString = f"$n%02d"

  private def readInput: String = {
    val source = Source.fromFile(s"input/$dayString.txt")
    try source.mkString finally source.close()
  }

  def run(): Unit = {
    runTestCase(day.test)
    runMainCase(readInput)
  }

  private def runTestCase(test: Result.Test): Unit = {
    val result = day.execute(test.input)
    assert(test.expected1 == result.part1, s"Part 1 expected result did not match actual result.\nExpected: ${test.expected1}\nActual: ${result.part1}")
    test.expected2.foreach(expectedB =>
      assert(expectedB == result.part2, s"Part 2 expected result did not match actual result.\nExpected: $expectedB\nActual: ${result.part2}")
    )
  }

  private def runMainCase(input: String): Unit = {
    val stopWatch = StopWatch.start
    val result = day.execute(input)
    val executionMs = stopWatch.stop.millis
    println(s"Day $dayString: (Part 1: ${result.part1}, Part 2: ${result.part2})  [${executionMs}ms]")
  }
}
