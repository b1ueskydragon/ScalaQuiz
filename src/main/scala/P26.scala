import scala.collection.mutable

object P26 {
  def main(args: Array[String]): Unit = {
    val sample = List('a, 'b, 'c)
    println(combinations(2, sample))

    // generate all the possibilities C(N,K)
    // List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  }

  // all of each elements (type List[A]) length is same as `n`.

  def combinations[A](n: Int, origin: List[A]): List[List[A]] = {
    val outer = new mutable.MutableList[List[A]]

    def recursion(current: List[A], eachList: List[A]): mutable.MutableList[List[A]] =
      current match {

        case _ if eachList.size == n => outer += eachList
        case h :: tail if current.size + eachList.size >= n =>
          recursion(tail, h :: eachList)
          recursion(tail, eachList)
        case _ => outer
      }

    recursion(origin, List()).toList
  }
}
