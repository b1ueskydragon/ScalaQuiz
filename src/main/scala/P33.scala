object P33 {
  // Two numbers are coprime if their gcd equals 1.
  def main(args: Array[String]) = {
    val a = 35
    val b = 64
    println(new P33(a).isCoprimeTo(b))
  }
}

class P33(val given: Int) {
  import P32._
  def isCoprimeTo(n: Int): Boolean = gcd(given, n) == 1
}
