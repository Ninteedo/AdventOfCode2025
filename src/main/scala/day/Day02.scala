package day

import run.{DayRunner, Result}
import utility.*

class Day02 extends IDay(2) {
  override def execute(input: String): Result = {
    val idRanges = input.trim.split(",").map(IdRange.parse).toList
    Result(part1(idRanges), part2(idRanges))
  }

  private def part1(idRanges: List[IdRange]): Long = {
    def checkInvalid(n: Long): Boolean = {
      val len = n.digitCount
      len.isEven && {
        val mag = 10L.pow(len.half)
        n % mag == n / mag
      }
    }

    sumInvalidIds(idRanges, checkInvalid)
  }

  private def part2(idRanges: List[IdRange]) = {
    val factorCache = Cache[Long, List[Long]]

    def checkInvalid(n: Long): Boolean = {
      val len = n.digitCount
      val factors = factorCache.getOrCompute(len, len.factors.init.toList)
      factors.exists(digits =>
        val mag = 10L.pow(digits)
        val c = len / digits
        val base = n % mag
        (1 until c.toInt).forall(c => (n / mag.pow(c)) % mag == base)
      )
    }

    sumInvalidIds(idRanges, checkInvalid)
  }

  private def sumInvalidIds(idRanges: List[IdRange], checkInvalid: Long => Boolean): Long = {
    idRanges.flatMap(_.filter(checkInvalid)).sum
  }

  private case class IdRange(start: Long, end: Long) {
    private val range: Seq[Long] = start to end

    def filter(predicate: Long => Boolean): Seq[Long] = range.filter(predicate)
  }

  private object IdRange {
    def parse(s: String): IdRange = {
      val (l, r) = s.splitPair("-")
      IdRange(l.toLong, r.toLong)
    }
  }

  val test = Result.Test(1227775554, Some(4174379265L), "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
}

object Day02 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day02()).run()
  }
}
