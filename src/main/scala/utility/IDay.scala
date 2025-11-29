package utility

trait IDay(val dayNumber: Int) {
  def execute(input: String): Result

  protected val incomplete: String = "INCOMPLETE"
}
