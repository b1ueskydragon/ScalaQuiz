import scala.annotation.tailrec

/** without using P09#pack. do it directly.
  */
object P13 {
  def main(args: Array[String]): Unit = {
    val given = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encodeDirect(given))
    println(encodeDirect_(given))
  }

  def encodeDirect[A](l: List[A]): List[(Int, A)] = {
    @tailrec
    def rec(acc: List[(Int, A)], xs: List[A]): List[(Int, A)] = xs match {
      case Nil => acc.reverse
      case h :: tail if acc.isEmpty || h != acc.head._2 => rec((1, h) :: acc, tail)
      case h :: tail => rec((acc.head._1 + 1, h) :: acc.tail, tail)
    }

    rec(List(), l)
  }

  def encodeDirect_[A](l: List[A]): List[(Int, A)] = {
    @tailrec
    def rec(acc: List[(Int, A)], xs: List[A]): List[(Int, A)] = xs match {
      case Nil => acc.reverse // to prevent Exception caused by Nil.head
      case _ =>
        val (pack, unpack) = xs.span(_ == xs.head)
        rec((pack.length, xs.head) :: acc, unpack)
    }

    rec(List(), l)
  }
}
