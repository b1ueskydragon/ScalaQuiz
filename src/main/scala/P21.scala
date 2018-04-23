object P21 {
  def main(args: Array[String]): Unit = {
    val origin = List('a, 'b, 'c, 'd)
    println(insertAt('new, 1, origin)) // List('a, 'new, 'b, 'c, 'd)
  }

  def insertAt[A](newEl: A, place: Int, l: List[A]): List[A] = {
    def recursion(p: Int, rst: List[A], ori: List[A]): List[A] = ori match {
      case Nil => rst
      case h :: tail if p < place =>
        println(h)
        recursion(p + 1, rst :+ h, tail)
      case h :: tail if p == place =>
        println(h)
        recursion(p + 1, rst :+ newEl :+ h, tail)
      case _ =>
        println(ori.head)
        recursion(p + 1, rst ::: ori, Nil) // 残りはそのままくっつける
    }

    recursion(0, Nil, l) // TODO reverse ver
  }
}
