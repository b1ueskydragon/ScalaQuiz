import P10._

object P11 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    // List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

    println(_encodeModified(List(), encode(target)))
  }

  // Modify the result of problem P10
  // param `l` should be target
  def encodeModified[A](l: List[A]) = {
  }

  // param `tupList` should be result of P10#encode
  // return type is List[Any] ( != List[A])
  def _encodeModified[A](result: List[Any], tupList: List[(Int, A)]): List[Any] = tupList match {
    case Nil => result
    case h :: tail
      if h._1 == 1 =>
      _encodeModified(result ::: List(h._2), tail)
    case h :: tail =>
      _encodeModified(result ::: List(h), tail)
  }
}
