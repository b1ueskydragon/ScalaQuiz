import scala.annotation.tailrec

/** without using P09#pack. do it directly.
  */
object P13 {
  def main(args: Array[String]): Unit = {
    val given = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encodeDirect(given))
    println(encodeDirect_(given))
    println(encodeDirect__(given))
  }

  def encodeDirect[A](l: List[A]): List[(Int, A)] = {
    @tailrec
    def rec(acc: List[(Int, A)], xs: List[A]): List[(Int, A)] = xs match {
      case Nil => acc
      case h :: tail if acc.isEmpty || h != acc.last._2 => rec(acc ::: List((1, h)), tail)
      case h :: tail => rec(acc.init ::: List((acc.last._1 + 1, h)), tail)
    }

    rec(List(), l)
  }

  def encodeDirect_[A](l: List[A]): List[(Int, A)] = {
    @tailrec
    def rec(acc: List[(Int, A)], xs: List[A]): List[(Int, A)] = xs match {
      case Nil => acc // to prevent Exception caused by Nil.head
      case _ =>
        val (pack, unpack) = xs.span(_ == xs.head)
        rec(acc ::: List((pack.length, xs.head)), unpack)
    }

    rec(List(), l)
  }

  // foldr to avoid use ::: or reverse
  def encodeDirect__[A](xs: List[A]): List[(Int, A)] = xs.foldRight(List[(Int, A)]()) {
    (x, acc) => if (acc.isEmpty || acc.head._2 != x) (1, x) :: acc else (acc.head._1 + 1, x) :: acc.tail
  }

}
