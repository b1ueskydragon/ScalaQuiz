object P41 {
  // Given a range and print a list of all even numbers and their Goldbach composition.
  def main(args: Array[String]): Unit = {
    printGoldbachList(9 to 20)

    // Find out how many both primes are bigger than 50 cases in the range 2..3000.
    //  printGoldbachListLimited(1 to 2000, 50)
    //  992 = 73 + 919
    //  1382 = 61 + 1321
    //  1856 = 67 + 1789
    //  1928 = 61 + 1867
  }

  def printGoldbachList(range: Range) {
    range.filter(_ % 2 == 0).foreach { k =>
      val pair = goldbach(k)
      println(s"""$k = ${pair._1} + ${pair._2}""")
    }
  }

  def printGoldbachListLimited(range: Range, low: Int) {
    // TODO
  }

  def goldbach(even: Int): (Int, Int) = {
    import P31._
    val primes = Stream.from(3, 2).filter(isPrime).take(even)

    primes.takeWhile(_ < even).find(p => isPrime(even - p)) match {
      case Some(p) => (p, even - p)
      case _ => throw new IllegalArgumentException
    }
  }
}
