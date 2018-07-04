object P33 {
  // Two numbers are coprime if their gcd equals 1.
  def main(args: Array[String]) = {
    val a = 35
    val b = 64
    println(isCoprimeTo(a, b))
  }

  import P32._
  def isCoprimeTo(a: Int, b: Int): Boolean = if (gcd(a, b) == 1) true else false
}
