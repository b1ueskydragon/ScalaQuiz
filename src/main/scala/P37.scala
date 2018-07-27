import scala.math.pow
import P36._

object P37 {
  def main(args: Array[String]): Unit = {
    lazy val given = new java.util.Scanner(System.in).nextInt()
    lazy val res00 = generalTerm(given)
    // println(res00)
    lazy val res01 = phi(given)
    println(res01)
    lazy val res02 = totientFast(given)
    println(res02)
  }

  // use folding
  def totientFast(n: Int): Int = {
    // s 起点の引数, t タプル
    primeFactorMultiplicityList(n).foldLeft(1) { (s, t) =>
      t match {
        case (p, m) => s * (p - 1) * pow(p, m - 1).toInt
      }
    }
  }

  // general term: (Pk - 1) x Pk^(Mk-1)
  def phi(n: Int): Double = {
    generalTerm(n).map { t =>
      val p = t._1
      val m = t._2
      (p - 1) * pow(p, m - 1)
    }.product
  }

  // the list of prime factors (and their multiplicities) of a given number N.
  // [[P1, M1], [P2, M2], [P3, M3], ...]
  def generalTerm(N: Int): List[(Int, Int)] = primeFactorMultiplicityList(N)
}
