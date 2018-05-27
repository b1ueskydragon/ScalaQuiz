object P32 {
  //  Use Euclid's algorithm. (長方形を正方形で埋め尽くす)
  def main(args: Array[String]) = {
    println(gcd(36, 63))
  }

  def gcd(d: Int, m: Int): Int = m match {
    case n if n == 0 => d
    case _ => gcd(m, d % m)
  }
}
