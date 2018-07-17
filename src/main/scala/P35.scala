import P31._

object P35 {
  //  Construct a flat list containing the prime factors in ascending order.
  def main(args: Array[String]): Unit = {
    val given = new java.util.Scanner(System.in).nextInt() // 315
    val res00 = primeFactors(given)
    println(res00) // List(3, 3, 5, 7)
    println(res00.product)
  }

  /**
    * 最初単位で割り続ける. やりきってから次の head に進める.
    *
    * @param n dividend
    * @return
    */
  def primeFactors(n: Int): List[Int] = {
    val primes = (2 to n).filter(isPrime).toList

    def _rec(n: Int, div: List[Int]): List[Int] = {
      if (isPrime(n)) List(n) // exit case
      else if (n % div.head == 0) div.head :: _rec(n / div.head, div) // s1) 除数で割り切れる場合
      else _rec(n, div.tail) // s2) 除数で割り切れない場合
    }

    _rec(n, primes)
  }
}
