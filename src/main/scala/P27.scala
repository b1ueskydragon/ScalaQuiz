import P26.combinations

import scala.collection.mutable

object P27 {
  def main(args: Array[String]): Unit = {
    val sample = List("A", "B", "C", "D", "E", "F", "G", "H", "I")
    // println(group3(sample))

    val pattern = List(2, 3, 4)
    println(group(pattern, sample))
  }

  // determine first (_,_) (_,_,_) then can get (_,_,_,_) automatically
  def group3[A](origin: List[A]): List[List[List[A]]] = {
    for {
      _2 <- combinations(2, origin)
      tail = origin diff _2
      _3 <- combinations(3, tail)
    } yield List(_2, _3, tail diff _3)
  }

  // specify a list of group sizes and the predicate will return a list of groups
  def group[A](patterns: List[Int], origin: List[A]): List[Any] = {

    // each is each element of outerRes.
    def _rec(p: List[Int], l: List[Any], each: List[Any], outerRes: List[Any]): List[Any] = p match {
      case h :: tail =>
        val seq = combinations(h, l)
        seq.head :: _rec(tail, l, each, each :: outerRes)
      case Nil => outerRes
    }

    _rec(patterns, origin, Nil, Nil)
  }

  // deal with only 3-lengthed-pattern-list
  def _group[A](patterns: List[Int], origin: List[A]): List[Any] = {
    for {
      a <- combinations(patterns.head, origin)
      tail = origin diff a
      b <- combinations(patterns.tail.head, tail)
      // Pattern should be generated dynamically
    } yield List(a, b, tail diff b)
  }
}
