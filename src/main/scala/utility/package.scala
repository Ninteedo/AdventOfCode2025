import java.util.regex.Pattern
import scala.annotation.tailrec

package object utility {
  extension [T](it: IterableOnce[T]) {
    def distinctCount: Int = it.iterator.distinct.size

    def combinationPairs: Iterator[(T, T)] = it.iterator.toList.combinations(2).map(l => (l(0), l(1)))

    def slidingPairs: Iterator[(T, T)] = it.iterator.sliding(2).map(elements => (elements(0), elements(1)))
  }

  extension (b: Boolean) {
    /**
     * Converts a [[Boolean]] to an [[Int]].
     * @return 1 for `true` or 0 for `false`
     */
    def toInt: Int = if b then 1 else 0

    /**
     * @param a  value
     * @tparam A type of [[a]]
     * @return [[Some(a)]] if `true`, otherwise [[None]]
     */
    def thenOption[A](a: => A): Option[A] = if b then Some(a) else None
  }

  extension (n: Int) {
    def inRangeInclusive(min: Int, max: Int): Boolean = n >= min && n <= max

//    def isNonNegative: Boolean = n >= 0
//
//    def isPositive: Boolean = n > 0
//
//    def isEven: Boolean = n % 2 == 0
//
//    def isOdd: Boolean = n % 2 == 1

//    def half: Int = n / 2

//    def digitCount: Int = n.toString.length - (if (n.isNonNegative) 0 else 1)
  }

  extension (n: Long) {
    def inRangeInclusive(min: Long, max: Long): Boolean = n >= min && n <= max

    def inRangeExclusive(min: Long, max: Long): Boolean = n >= min && n < max

    def isNonNegative: Boolean = n >= 0

    def isPositive: Boolean = n > 0

    def isEven: Boolean = n % 2 == 0

    def isOdd: Boolean = n % 2 == 1

    def half: Long = n / 2

    def square: Long = n * n

    @tailrec
    def digitCount: Int = {
      if n == 0 then 1
      else if n >= 0 then Math.log10(n.toDouble).toInt + 1
      else if n == Long.MinValue then 19 else (-n).digitCount
    }

    def factors: LazyList[Long] = {
      if (n < 1) throw new IllegalArgumentException("Can only calculate factors of a positive number")
      else LazyList.from(1L to n).filter(m => n % m == 0)
    }

    def pow(b: Long): Long = {
      @tailrec
      def loop(exp: Long, base: Long, acc: Long): Long = {
        if exp == 0 then acc
        else if (exp & 1L) == 1L then loop(exp >>> 1, base * base, acc * base)
        else loop(exp >>> 1, base * base, acc)
      }

      loop(b, n, 1L)
    }
  }

  extension (s: String) {
    def splitPair(seperator: String | Pattern): (String, String) = {
      val sep = seperator match {
        case s: String => Pattern.compile(s)
        case p: Pattern => p
      }
      val splits = sep.split(s, 2)
      (splits(0), splits(1))
    }
  }

  def enumParse[T, K](values: Array[T], accessor: T => K)(key: K): T = values.find(v => accessor(v) == key) match {
    case Some(value) => value
    case None => throw new IllegalArgumentException(s"'$key' does not correspond to any value.")
  }
}
