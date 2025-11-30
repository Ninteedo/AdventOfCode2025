package day

import run.{DayRunner, Result}
import utility.{Helper, IDay}

class Day00 extends IDay(0) {
  override def execute(input: String): Result = {
    val result: Iterable[String] = Helper.readLines(input, identity)
    Result(result.head, result.tail.head)
  }

  val test = Result.Test("Foo\nBar", "Foo", Some("Bar"))
}

object Day00 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day00()).run()
  }
}
