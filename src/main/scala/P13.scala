object P13 {
  def main(args: Array[String]): Unit = {

    val target = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(_encodeDirect(List(), target)) // List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

    //Implement the so-called run-length encoding data compression method directly.
    // I.e. don't use other methods you've written (like P09's pack); do all the work directly.
  }

  def _encodeDirect[A](result: List[(Int, A)], rawList: List[A]): List[(Int, A)] = rawList match {
    case h :: tail if result.nonEmpty && h == result.last._2 =>
      println(h + " standard case 1")
      _encodeDirect(result.init ::: List((result.last._1 + 1, h)), tail)

    case h :: tail =>
      println(h + " standard case 2")
      _encodeDirect(result ::: List((1, h)), tail)

    case Nil =>
      println("tail empty")
      result
  }
}
