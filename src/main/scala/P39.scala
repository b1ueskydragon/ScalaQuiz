import P31._

object P39 {
  // Given a range of integers by its lower and upper limit,
  // construct a list of all prime numbers in that range.
  def main(args: Array[String]): Unit = {
    lazy val given = 7 to 31
    println(listPrimesinRange(given)) // List(7, 11, 13, 17, 19, 23, 29, 31)
  }

  def listPrimesinRange(range: Range): List[Int] = {
    range.filter(isPrime_).toList
  }
}
