object P41 {
  // Given a range of integers by its lower and upper limit,
  // print a list of all even numbers and their Goldbach composition.
  def main(args: Array[String]): Unit = {
    lazy val res00 = goldbach(10)
    lazy val res01 = goldbach(12)
    println(res00)
    println(res01)

    // e.g)
    //  printGoldbachList(9 to 20)
    // 10 = 3 + 7
    // 12 = 5 + 7
    // 14 = 3 + 11
    // 16 = 3 + 13
    // 18 = 5 + 13
    // 20 = 3 + 17
  }

  def printGoldbachList(range: Range): Unit = {
    // TODO
  }

  def goldbach(even: Int): (Int, Int) = {
    import P31._
    val primes = Stream.from(3, 2).filter(isPrime).take(even)

    primes.takeWhile(_ < even).find(p => isPrime(even - p)) match {
      case Some(p) => (p, even - p)
    }
  }
}
