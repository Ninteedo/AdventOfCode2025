package day

import utility.{Helper, IDay, Result}

class Day00 extends IDay(0) {
  override def execute(input: String): Result = {
    val result: Iterable[String] = Helper.readLines(input, identity)
    Result(result.head, result.tail.head)
  }
}
