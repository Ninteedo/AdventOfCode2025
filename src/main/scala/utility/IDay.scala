package utility

import run.Result

import scala.io.Source

trait IDay(val dayNumber: Int) {
  def execute(input: String): Result

  protected val incomplete: String = "INCOMPLETE"

  def test: Result.Test
}
