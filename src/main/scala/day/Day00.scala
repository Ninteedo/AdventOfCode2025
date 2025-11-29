package day

import utility.{Helper, IDay}

class Day00 extends IDay(0) {
  override def execute(input: String): (String, String) = {
    val result: Iterable[String] = Helper.readLines(input, identity)
    (result.head, result.tail.head)
  }
}
