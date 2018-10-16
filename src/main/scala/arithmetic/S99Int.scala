package arithmetic {

  class S99Int(val num: Int) {

    import S99Int._

    // Lazy infinite list of primes (Stream is lazy).
    val primes: Stream[Int] = Stream.cons(hd = 2, tl = Stream.from(3, 2).filter(_.isPrime))

    // Put a limit on `isPrime` to the square root.
    def isPrime: Boolean = (num > 1) && (primes.takeWhile { ph => ph <= Math.sqrt(num) } forall { ph => num % ph != 0 })

    // Euclid's algorithm.
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  }

  object S99Int {
    /** @param num Default is 0 */
    implicit def int2S99Int(num: Int = 0): S99Int = new S99Int(num)

    def main(args: Array[String]): Unit = {
      // P31
      println(31.isPrime)

      // P32
      println(int2S99Int().gcd(12, 40))

    }
  }

}

