object P32 {
  //  Use Euclid's algorithm. (長方形を正方形で埋め尽くす)
  def main(args: Array[String]) = {
    val a = 36
    val b = 63
    println(gcd(a, b))
    println(gcd01(a, b))
  }

  def gcd(a: Int, b: Int): Int = b match {
    case n if n == 0 => a
    case _ => gcd(b, a % b)
  }

  def gcd01(a: Int, b: Int): Int = if (b == 0) a else gcd01(b, a % b)
}
