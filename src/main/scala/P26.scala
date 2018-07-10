import scala.collection.mutable

object P26 {
  def main(args: Array[String]): Unit = {
    val sample = List('a, 'b, 'c, 'd, 'e, 'f)
    println(combinations(3, sample))

    // generate all the possibilities C(N,K)
    // List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  }

  // all of each elements (type List[A]) length is same as `n`.
  // eachList would be each stack.
  def combinations[A](n: Int, origin: List[A]): List[List[A]] = {
    val outer = new mutable.MutableList[List[A]]

    def recursion(current: List[A], eachList: List[A]): outer.type = current match {
      case _ if eachList.length == n => outer += eachList.reverse // concat to Result
      case h :: tail =>
        recursion(tail, h :: eachList)
        recursion(tail, eachList)
      case _ => outer // nothing to do. case of current and eachList cannot generate `n` length parts.
    }

    recursion(origin, List()).toList
  }
}
