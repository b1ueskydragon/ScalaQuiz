import P31._

object P35 {
  //  Construct a flat list containing the prime factors in ascending order.
  def main(args: Array[String]): Unit = {
    val given = new java.util.Scanner(System.in).nextInt() // 315
    val res00 = primeFactors_(given)
    println(res00) // List(3, 3, 5, 7)
    println(res00.product)
  }

  def primeFactors_(n: Int): List[Int] = {
    val primes = (2 to n).filter(isPrime).toList

    def _rec(n: Int, div: List[Int]): List[Int] = div match {
      case _ if isPrime(n) => List(n) // exit case
      case h :: tail if n % h == 0 => h :: _rec(n / h, tail) // s1) 除数で割り切れる場合
      case _ => _rec(n, primes.tail) // s2) 除数で割り切れない場合
    }

    _rec(n, primes).sortWith(_ < _)
  }
}
