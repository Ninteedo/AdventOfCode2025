package utility

import run.Result

import scala.io.Source

trait IDay(val dayNumber: Int) {
  def execute(input: String): Result

  def test: Result.Test
}

object IDay {
  val incomplete: String = "INCOMPLETE"
}
