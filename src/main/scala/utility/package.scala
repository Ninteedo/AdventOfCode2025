import java.util.regex.Pattern

package object utility {
  extension [T](it: IterableOnce[T]) {
    def distinctCount: Int = it.iterator.distinct.size

    def combinationPairs: Iterator[(T, T)] = it.iterator.toList.combinations(2).map(l => (l(0), l(1)))
  }

  extension (b: Boolean) {
    def toInt: Int = if b then 1 else 0

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

    def half: Int = n / 2

    def digitCount: Int = n.toString.length - (if (n.isNonNegative) 0 else 1)
  }

  extension (n: Long) {
    def inRangeInclusive(min: Long, max: Long): Boolean = n >= min && n <= max

    def isNonNegative: Boolean = n >= 0

    def isPositive: Boolean = n > 0

    def isEven: Boolean = n % 2 == 0

    def isOdd: Boolean = n % 2 == 1

    def half: Long = n / 2

    def digitCount: Int = n.toString.length - (if (n.isNonNegative) 0 else 1)
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
