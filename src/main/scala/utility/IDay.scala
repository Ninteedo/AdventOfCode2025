package utility

trait IDay(val dayNumber: Int) {
  def execute(input: String): (Any, Any)

  protected val incomplete: String = "INCOMPLETE"
}
