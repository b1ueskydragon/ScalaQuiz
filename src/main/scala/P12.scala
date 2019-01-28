import scala.annotation.tailrec

object P12 {
  def main(args: Array[String]): Unit = {
    val given = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    println(decode(given))
  }

  def decode[A](l: List[(Int, A)]): List[A] = {
    @tailrec
    def rec(res: List[A], xs: List[(Int, A)]): List[A] = xs match {
      case Nil => res.reverse
      case h :: tail if h._1 > 1 => rec(h._2 :: res, (h._1 - 1, h._2) :: tail)
      case h :: tail => rec(h._2 :: res, tail)
    }

    rec(List(), l)
  }
}
