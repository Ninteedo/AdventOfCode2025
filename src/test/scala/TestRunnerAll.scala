object TestRunnerAll {
  def main(args: Array[String]): Unit = {
    val results = LazyList.from(TestRunner.dayRunners).map(TestRunner.getDayResultFromRunner)
    results.map(_._1).foreach(println)
    val totalTime: Long = results.map(_._2).sum
    println(s"Total execution time: ${totalTime}ms")
  }
}
