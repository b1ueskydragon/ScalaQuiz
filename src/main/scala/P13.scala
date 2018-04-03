object P13 {
  def main(args: Array[String]): Unit = {

    val target = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encodeDirect(target))
  }

  /**
    * not using P09#pack ver
    *
    * @param rawList
    * @tparam A
    * @return
    */
  def encodeDirect[A](rawList: List[A]): List[(Int, A)] = {
    def _encodeDirect(result: List[(Int, A)], rawList: List[A]): List[(Int, A)] = rawList match {
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

    _encodeDirect(List(), rawList)
  }
}
