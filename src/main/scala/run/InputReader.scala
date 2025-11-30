package run

import scala.io.Source

case class InputReader(dayNumber: Int) {
  def read: String = {
    val dayString = f"$dayNumber%02d"
    val source = Source.fromFile(s"input/$dayString.txt")
    try source.mkString finally source.close()
  }
}
