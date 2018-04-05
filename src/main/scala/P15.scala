object P15 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'c, 'd)
    println(duplicateN(3, List(), target))
    //  List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  def duplicateN[A](n: Int, result: List[A], list: List[A]): List[A] = list match {
    case Nil => result
    case h :: _ if n > 1 => duplicateN(n - 1, result ::: List(h), list)
    case h :: tail => duplicateN(3, result ::: List(h), tail)
  }
}
