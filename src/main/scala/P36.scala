import P31._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
    val primes = (2 to n).filter(isPrime).toList
    val outer = new ListBuffer[(Int, Int)]

    def _rec(n: Int, div: List[Int]): Unit = {
      if (isPrime(n)) {
        outer += ((n, 1))
      }
      else if (n % div.head == 0) {
        outer += ((div.head, 1))
        _rec(n / div.head, div)
      }
      else {
        _rec(n, div.tail)
      }
    }

    _rec(n, primes)
    outer.toList
  }

//  // p は昇順に到着する e.g.) 2, 3, 3, 5 ...
//  def pack(p: Int): List[(Int, Int)] = {
//    def _rec(p: Int, cnt: Int, outer: List[(Int, Int)]): List[(Int, Int)] = {
//      if (p != outer.last._1) _rec(p, cnt, outer ::: List((p, 1)))
//      else _rec(p, cnt + 1, outer.init ::: List((p, cnt)))
//    }
//
//    _rec(p, 1, List())
//  }

  //  def primeFactorMultiplicityMap(n: Int): Map[Int,Int] = {
  //  }
}
