object P33 {
  // Two numbers are coprime if their gcd equals 1. (互いに素)
  def main(args: Array[String]) = {
    val a = 35
    val b = 50
    println(isCoprimeTo(a, b))
  }

  import P32._
  def isCoprimeTo(given: Int, n: Int): Boolean = gcd(given, n) == 1
}



