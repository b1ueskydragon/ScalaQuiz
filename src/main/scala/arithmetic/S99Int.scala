package arithmetic {

  class S99Int(val start: Int) {
    import S99Int._

    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    gcd(36, 63)

    def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1
    35.isCoprimeTo(64)

    def totient: Int = (1 to start).count(isCoprimeTo)
    10.totient
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  }

}