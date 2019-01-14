import P10._

import scala.annotation.tailrec

/** Modify the result of problem P10
  * return type is List[Any]
  */
object P11 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encodeModified(encode(target)))
    println(encodeModified_(target))
  }

  def encodeModified[A](l: List[(Int, A)]): List[Any] = {
    @tailrec
    def rec(res: List[Any], pairs: List[(Int, A)]): List[Any] = pairs match {
      case Nil => res.reverse
      case h :: tail if h._1 == 1 => rec(h._2 :: res, tail)
      case h :: tail => rec(h :: res, tail)
    }

    rec(List(), l)
  }

  def encodeModified_[A](l: List[A]): List[Any] = encode(l) map {
    case e if e._1 == 1 => e._2
    case e => e
  }
}
