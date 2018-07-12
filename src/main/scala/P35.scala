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
    primesToN.filter(n % _ == 0)
  }
}
