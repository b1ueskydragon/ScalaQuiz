import P31._

object P41 {
  // Given a range and print a list of all even numbers and their Goldbach composition.
  def main(args: Array[String]): Unit = {

    // printGoldbachList(9 to 20)

    // Find out how many both primes are bigger than 50 cases(minimum value) in the given range.
    printGoldbachListLimited(2 to 3000, 50)

  }

  def printGoldbachList(range: Range) {
    range.filter(_ % 2 == 0).foreach { k =>
      val pair = goldbach(k)
      println(s"""$k = ${pair._1} + ${pair._2}""")
    }
  }

  def printGoldbachListLimited(range: Range, low: Int) {
    // 前処理
    range.filter(k => k % 2 == 0 && k > low * 2).foreach { k =>
      // 本処理
      primes.filter(_ > 2).takeWhile(_ < k).find(p => isPrime(k - p)) match {
        case Some(p) if p > low => println(s"""$k = $p + ${k - p}""")
        case _ =>
      }
    }
  }

  def goldbach(even: Int): (Int, Int) = {
    primes.filter(_ > 2).takeWhile(_ < even).find(p => isPrime(even - p)) match {
      case Some(p) => (p, even - p)
      case _ => throw new IllegalArgumentException
    }
  }
}
