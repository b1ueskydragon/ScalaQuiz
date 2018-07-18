import P35._

object P36 {
  // Construct a list containing the prime factors and their multiplicity.
  def main(args: Array[String]): Unit = {
    val given = new java.util.Scanner(System.in).nextInt() // 315

    val resList = primeFactorMultiplicityList(given)
    // val resMap = primeFactorMultiplicityMap(given)

    println(resList) // List((3,2), (5,1), (7,1))
    // println(resMap) // Map(3 -> 2, 5 -> 1, 7 -> 1)
  }

  def primeFactorMultiplicityList(n: Int): List[(Int, Int)] = {
    def _pack(ori: List[Int], out: List[(Int, Int)]): List[(Int, Int)] = ori match {
      case Nil => out
      case h :: tail if out.nonEmpty && out.last._1 == h => _pack(tail, out.init ::: List((h, out.last._2 + 1)))
      case h :: tail => _pack(tail, out ::: List((h, 1)))
    }

    _pack(primeFactors(n), Nil)
  }

  //  def primeFactorMultiplicityMap(n: Int): Map[Int,Int] = {
  //  }
}
