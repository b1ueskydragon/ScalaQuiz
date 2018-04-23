object P22 {
  def main(args: Array[String]): Unit = {
    println(range(4, 9)) //  List(4, 5, 6, 7, 8, 9)
  }

  def range(from: Int, to: Int): List[Int] = {
    def recursion(cnt: Int, start: Int, rst: List[Int]): List[Int] = cnt match {
      case _ if cnt <= to - from => recursion(cnt + 1, start + 1, rst :+ start)
      case _ => rst
    }

    recursion(0, from, Nil)
  }
}
