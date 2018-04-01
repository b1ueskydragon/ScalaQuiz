object P12 {
  def main(args: Array[String]): Unit = {
    val target = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    println(decode(target))
  }

  def decode[A](encoded: List[(Int, A)]): List[A] = {
    def _decode(result: List[A], encoded: List[(Int, A)]): List[A] = encoded match {
      case Nil => result
      case h :: tail
        if h._1 > 1 =>
        _decode(result ::: List(h._2), (h._1 - 1, h._2) :: tail)
      case h :: tail => _decode(result ::: List(h._2), tail)
    }

    _decode(List(), encoded)
  }
}
