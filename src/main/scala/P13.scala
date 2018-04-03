object P13 {
  def main(args: Array[String]): Unit = {

    val target = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(_encodeDirect(List(), target)) // List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

    //Implement the so-called run-length encoding data compression method directly.
    // I.e. don't use other methods you've written (like P09's pack); do all the work directly.
  }

  def _encodeDirect[A](result: List[(Int, A)], rawList: List[A]): List[(Int, A)] = rawList match {
    case Nil => result

    case h :: tail if tail.isEmpty => // rawList の末端
      println(h)
      result

    case h :: tail if h != tail.head || result.isEmpty => // 前後の要素が異なる or rawList の先端
      println(h)
      _encodeDirect(result ::: List((1, h)), tail)

    case h :: tail => // 前後の要素が同じ
      println(h)
      _encodeDirect(result.init ::: List((result.last._1 + 1, h)), tail)
  }
}
