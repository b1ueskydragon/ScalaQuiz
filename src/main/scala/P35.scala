import P31._

object P35 {
  //  Construct a flat list containing the prime factors in ascending order.
  def main(args: Array[String]): Unit = {
    val given = new java.util.Scanner(System.in).nextInt() // 315
    println(primeFactors_(given)) // List(3, 3, 5, 7)
  }

  def primeFactors(n: Int) = {
  }

  def primeFactors_(n: Int): List[Int] = {
    val primesToN = (2 to n).filter(isPrime).toList

    def _rec(n: Int, divisor: List[Int], res: List[Int]): List[Int] = divisor match {
      case _ if isPrime(n) => List(n) ::: res // exit case
      case h :: tail if n % h == 0 => _rec(n / h, tail, h :: res)
      case h :: _ => _rec(n / primesToN.head, primesToN, h :: res)
      // case List() => res // exit case
    }

    // _rec(n, primesToN, List()).reverse
    _rec(n, primesToN, List()).sortWith(_ < _)
  }
}
