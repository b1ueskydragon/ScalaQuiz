import P31._

object P40 {
  // Write a function to find the two prime numbers that sum up to a given even integer.
  def main(args: Array[String]): Unit = {
    val given = 28
    lazy val res = goldbach(given)
    println(res)
    lazy val res01 = golabachFast(given)
    println(res01)
  }

  def goldbach(even: Int): (Int, Int) = {
    val primes = Stream.from(3, 2).filter(isPrime_).take(even).toList // 2 is excluded

    // rights は最初の状態では left をも含む.
    def _rec(left: Int, rights: List[Int], origin: List[Int]): (Int, Int) = rights match {
      case h :: _ if left + h == even => (left, h) // exit case (成立)
      case h :: tail if left + h < even => _rec(left, tail, origin) // 不成立だけど余地あり
      case _ => _rec(origin.head, origin, origin.tail) // left そのもののカーソルを右に移動
    }

    _rec(primes.head, primes, primes)
  }

  def golabachFast(even: Int): (Int, Int) = {
    val primes = Stream.from(3, 2).filter(isPrime_).take(even) // 2 is excluded
    // p is a cursor
    primes.takeWhile(_ < even).find(p => isPrime(even - p)) match {
      case Some(p) => (p, even - p)
      case _ => throw new IllegalArgumentException
    }
  }
}
