package utility

import run.Result

trait IDay(val dayNumber: Int) {
  def execute(input: String): Result

  def test: Result.Test | List[Result.Test]
}

object IDay {
  val incomplete: String = "INCOMPLETE"
}
