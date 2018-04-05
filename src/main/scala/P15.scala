object P15 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'c, 'd)
    println(duplicateN(3, target))
  }

  def duplicateN[A](n: Int, ori: List[A]): List[A] = {
    val globalN = n

    def _duplicateN(n: Int, result: List[A], list: List[A]): List[A] = list match {
      case Nil => result
      case h :: _ if n > 1 => _duplicateN(n - 1, result ::: List(h), list)
      case h :: tail => _duplicateN(globalN, result ::: List(h), tail)
    }

    _duplicateN(globalN, List(), ori)
  }
}
