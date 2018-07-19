import P31.isPrime
import P35._

import scala.collection.mutable

object P36 {
  // Construct a list containing the prime factors and their multiplicity.
  def main(args: Array[String]): Unit = {
    val given = new java.util.Scanner(System.in).nextInt() // 315

    val resList = primeFactorMultiplicityList(given)
    println(resList) // List((3,2), (5,1), (7,1))

    val resMap = primeFactorMultiplicityMap(given)
    println(resMap) // Map(3 -> 2, 5 -> 1, 7 -> 1)
  }

  def primeFactorMultiplicityList(n: Int): List[(Int, Int)] = {
    def _pack(ori: List[Int], out: List[(Int, Int)]): List[(Int, Int)] = ori match {
      case Nil => out
      case h :: tail if out.nonEmpty && out.last._1 == h => _pack(tail, out.init ::: List((h, out.last._2 + 1)))
      case h :: tail => _pack(tail, out ::: List((h, 1)))
    }

    _pack(primeFactors(n), Nil)
  }

  // Do directly
  def primeFactorMultiplicityMap(n: Int): Map[Int, Int] = {
    val outer = new mutable.HashMap[Int, Int] // (n -> cnt)
    val primes = (2 to n).filter(isPrime).toList

    def _rec(n: Int, ps: List[Int], cnt: Int): Unit = {
      if (isPrime(n)) {
        if (outer.keySet.contains(n)) outer.update(n, cnt + 1)
        else outer.update(n, 1)
      }
      else if (n % ps.head == 0) {
        outer.update(ps.head, cnt + 1)
        _rec(n / ps.head, ps, cnt + 1)
      }
      else _rec(n, ps.tail, 0)
    }

    _rec(n, primes, 0)
    outer.toMap
  }
}
