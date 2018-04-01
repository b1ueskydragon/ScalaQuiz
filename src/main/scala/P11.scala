import P10._

object P11 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encodeModified(encode(target)))
    println(encodeModifiedStraight(target))
  }

  // Modify the result of problem P10
  // param `l` should be target
  def encodeModified[A](l: List[(Int, A)]): List[Any] = {
    // param `tupList` should be result of P10#encode
    // return type is List[Any] ( != List[A])
    def _encodeModified(result: List[Any], tupList: List[(Int, A)]): List[Any] = tupList match {
      case Nil => result
      case h :: tail
        if h._1 == 1 =>
        _encodeModified(result ::: List(h._2), tail)
      case h :: tail =>
        _encodeModified(result ::: List(h), tail)
    }

    _encodeModified(List(), l)
  }

  def encodeModifiedStraight[A](l: List[A]): List[Any] = encode(l) map {
    case e if e._1 == 1 => e._2
    case e => e
  }
}
