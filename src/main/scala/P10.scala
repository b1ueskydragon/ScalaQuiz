import scala.annotation.tailrec

object P10 {
  def main(args: Array[String]): Unit = {
    val given = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encode(given))
    println(encode_(given))
  }

  def encode[A](l: List[A]): List[(Int, A)] = {
    import P09.pack__
    @tailrec
    def rec(res: List[(Int, A)], xs: List[List[A]]): List[(Int, A)] = xs match {
      case Nil => res
      case h :: tail => rec(res ::: List((h.length, h.head)), tail)
    }

    rec(List(), pack__(l))
  }

  def encode_[A](l: List[A]): List[(Int, A)] = l.foldRight(List[(Int, A)]()) { (x, acc) =>
    acc match {
      case h :: tail if x == h._2 => (h._1 + 1, x) :: tail
      case _ => (1, x) :: acc
    }
  }

}
