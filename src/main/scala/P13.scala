object P13 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encodeDirect(target))
    println()
    println(encodeDirectUseSpan(List(), target))
  }

  /**
    * not using P09#pack ver.
    *
    * @param rawList original list
    * @tparam A type
    * @return packed list
    */
  def encodeDirect[A](rawList: List[A]): List[(Int, A)] = {
    def _encodeDirect(result: List[(Int, A)], rawList: List[A]): List[(Int, A)] = rawList match {
      case Nil =>
        println("tail empty")
        result

      case h :: tail if result.nonEmpty && h == result.last._2 =>
        println(h + " standard case 1")
        _encodeDirect(result.init ::: List((result.last._1 + 1, h)), tail)

      case h :: tail =>
        println(h + " standard case 2")
        _encodeDirect(result ::: List((1, h)), tail)
    }

    _encodeDirect(List(), rawList)
  }

  /**
    * using span function ver.
    *
    * write case `rawList == Nil` at first
    * (to prevent Exception caused by Nil.head).
    *
    * @param result  packed list
    * @param rawList original list
    * @tparam A type
    * @return result
    */
  def encodeDirectUseSpan[A](result: List[(Int, A)], rawList: List[A]): List[(Int, A)] = {
    rawList match {
      case Nil => result
      case _ =>
        println(rawList.head + " now looking")
        val (pack, unpack) = rawList span (_ == rawList.head)
        encodeDirectUseSpan(result ::: List((pack.length, rawList.head)), unpack)
    }
  }
}
