object P15 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'c, 'd)
    println(duplicateN(3, target))
    println()
    println(duplicateNUseFill(3, target))

  }

  def duplicateN[A](n: Int, ori: List[A]): List[A] = {
    val globalN = n

    def _duplicateN(n: Int, result: List[A], ori: List[A]): List[A] = ori match {
      case Nil => result
      case lst if n > 1 => _duplicateN(n - 1, result ::: List(lst.head), ori)
      case h :: tail => _duplicateN(globalN, result ::: List(h), tail)
    }

    _duplicateN(globalN, List(), ori)
  }

  def duplicateNUseFill[A](n: Int, ori: List[A]): List[A] = ori.flatMap {
    e => List.fill(n)(e)
  }
}
