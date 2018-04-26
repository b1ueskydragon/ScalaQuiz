import scala.util.Random

import P23.randomSelectOnce

object P24 {
  def main(args: Array[String]): Unit = {
    println(lottoFunc(6, 49))
    println(lottoRange(6, 49))
    println(lottoR(6, 49))

    // f(n, h), h P h-n+1
    //e.g List(23, 1, 17, 33, 21, 37)
  }

  // use Func
  def lottoFunc(n: Int, high: Int): List[Int] = Random.shuffle(1 to high toList).take(n)

  // use P23
  def lottoRange(n: Int, high: Int): List[Int] = randomSelectOnce(n, List.range(1, high))

  // recursion but duplication occurs
  def lottoR(seat: Int, high: Int): List[Int] = {
    def _recursion(n: Int, res: List[Int]): List[Int] = {
      if (n > 0) {
        _recursion(n - 1, res :+ Random.nextInt(high))
      } else res
    }

    _recursion(seat, Nil)
  }

  //  def lottoRSeed(seat: Int, high: Int): List[Int] = {
  //
  //  }
}
