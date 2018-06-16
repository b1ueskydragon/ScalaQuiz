object P28 {
  def main(args: Array[String]): Unit = {
    val given = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    println(lsort(given)); println(lsortBy(given))
    // List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))

    println(lsortFreq(given))
    // List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
  }

  // a) sort the elements of the list according to their length.
  def lsort[A](lists: List[List[A]]): List[List[A]] = lists.map(l => (l, l.length)).sortWith(_._2 < _._2).map(_._1)
  def lsortBy[A](lists: List[List[A]]): List[List[A]] = lists.sortBy(_.length)

  // b) sort the elements according to their length frequency (a more frequent length come later).
  def lsortFreq[A](lists: List[List[A]]) = {
    lists.map(_.length)
  }

}
