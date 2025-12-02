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
      len % 2 == 0 && {
        val mag = 10L.exponent(len / 2)
        n % mag == n / mag
      }
    }

    sumInvalidIds(idRanges, checkInvalid)
  }

  private def part2(idRanges: List[IdRange]) = {
    def checkInvalid(n: Long): Boolean = {
      val s = n.toString
      val len = s.length
      val factors = len.factors.init.toList
      (1 to len).exists(digits => factors.contains(digits) && s == s.substring(0, digits).repeat(len / digits))
    }

    sumInvalidIds(idRanges, checkInvalid)
  }

  private def sumInvalidIds(idRanges: List[IdRange], checkInvalid: Long => Boolean): Long = {
    idRanges.flatMap(idRange => (idRange.start to idRange.end).filter(checkInvalid)).sum
  }

  private case class IdRange(start: Long, end: Long)

  private object IdRange {
    def parse(s: String): IdRange = {
      val (l, r) = s.splitPair("-")
      IdRange(l.toLong, r.toLong)
    }
  }

  val test = Result.Test("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124", 1227775554, Some(4174379265L))
}

object Day02 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day02()).run()
  }
}
