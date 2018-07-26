//Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
object P34 {
  def main(args: Array[String]): Unit = {
    val n = 10
    println(totient(n)) // 4
  }
  import P33._
  def totient(given: Int): Int = (1 to given).count(isCoprimeTo(given, _)) // same as `filter and size`
}
