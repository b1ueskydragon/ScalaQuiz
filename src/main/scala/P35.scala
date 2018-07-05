object P35 {
  //  Construct a flat list containing the prime factors in ascending order.
  def main(args: Array[String]): Unit = {
    val given = 3156
    println(primeFactors(given)) // List(3, 3, 5, 7)
  }

  def primeFactors(n: Int): List[Int] = {
    import P31._
    val primesToN = (2 to n).filter(isPrime).toList

    for {
      p <- primesToN
      if n % p == 0
    } yield p

    // TODO greedy, dp, ...
  }
}
