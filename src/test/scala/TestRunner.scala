import utility.{IDay, StopWatch}

import java.io.File
import java.net.URL
import scala.io.Source

object TestRunner {
  val dayRunners: Array[IDay] = {
    val classLoader = Thread.currentThread().getContextClassLoader
    val path = "day"
    val resources = classLoader.getResources(path.replace('.', '/')).asIterator()
    var resourcesList: List[URL] = Nil
    resources.forEachRemaining({x => resourcesList = x :: resourcesList})

    resourcesList.flatMap(resource => {
      val dir = new File(resource.toURI)

      if (!dir.exists || !dir.isDirectory) {
        Seq.empty
      } else {
        dir.listFiles
          .filter(_.getName.endsWith(".class"))
          .map(_.getName.replace(".class", ""))
          .flatMap { className =>
            val dayClass: Class[?] = Class.forName(s"$path.$className")
            if (dayClass.getInterfaces.contains(classOf[IDay])) {
              Some(className, dayClass.getDeclaredConstructor().newInstance().asInstanceOf[IDay])
            } else {
              None
            }
          }
      }
    }).sortBy(_._1).map(_._2).toArray
  }

  def main(args: Array[String]): Unit = {
    val n: Int = args(0).toInt
    println(getDayResult(n)._1)
  }

  def dayString(n: Int): String = if (n < 10) s"0$n" else s"$n"

  def getDayResult(n: Int): (String, Long) = getDayResultFromRunner(dayRunners(n))

  def getDayResultFromRunner(day: IDay): (String, Long) = {
    val n = day.dayNumber
    val source: Source = Source.fromFile(s"input/${dayString(n)}.txt")
    val input: String = try source.mkString finally source.close()
    val stopWatch = StopWatch.start
    val result = day.execute(input)
    val executionMs = stopWatch.stop.millis
    val msg = s"Day ${dayString(n)}: (Part 1: ${result.partA}, Part 2: ${result.partB})  [${executionMs}ms]"
    (msg, executionMs)
  }
}
