import P10.{encode_ => encode}

import scala.annotation.tailrec

/** Modify the result of problem P10
  * return type is List[Any]
  */
object P11 {
  def main(args: Array[String]): Unit = {
    val given = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encodeModified(encode(given)))
    println(encodeModified_(given))
    println(encodeModified__(given))
  }

  def encodeModified[A](l: List[(Int, A)]): List[Any] = {
    @tailrec
    def rec(res: List[Any], pairs: List[(Int, A)]): List[Any] = pairs match {
      case Nil => res
      case h :: tail if h._1 == 1 => rec(res ::: List(h._2), tail)
      case h :: tail => rec(res ::: List(h), tail)
    }

    rec(List(), l)
  }

  def encodeModified_[A](l: List[A]): List[Any] = encode(l) map {
    case e if e._1 == 1 => e._2
    case e => e
  }

  // for fun
  def encodeModified__[A](l: List[A]): List[Any] =
    l.foldRight(List[(Int, A)]()) { (x, acc) =>
      acc match {
        case h :: tail if x == h._2 => (h._1 + 1, x) :: tail
        case _ => (1, x) :: acc
      }
    }.foldRight(List[Any]()) { (x, acc) =>
      if (x._1 == 1) x._2 :: acc else x :: acc
    }

}
