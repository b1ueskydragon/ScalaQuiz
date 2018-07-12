import P31._

object P35 {
  //  Construct a flat list containing the prime factors in ascending order.
  def main(args: Array[String]): Unit = {
    val given = new java.util.Scanner(System.in).nextInt() // 315
    val res00 = primeFactors_(given)
    println(res00) // List(3, 3, 5, 7)
    println(res00.product)
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
    // TODO 奇数 ...
    // _rec(n, primesToN, List()).reverse
    _rec(n, primesToN, List()).sortWith(_ < _)
  }
}
