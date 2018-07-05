object P31 {
  def main(args: Array[String]): Unit = {
    println(isPrime(7))
    println(isPrime(1681))
  }

  // Reduce the amount of computation as much as possible.
  // Is there any other way ?
  def isPrime(num: Int): Boolean = num match {
    case _ if num == 2 => true
    case _ if num < 2 || num % 2 == 0 => false
    case _ => ((3 to Math.sqrt(num).toInt) by 2) forall (num % _ != 0)
  }
}
