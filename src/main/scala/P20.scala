object P20 {
  def main(args: Array[String]): Unit = {
    val xs = List('a, 'b, 'c, 'd)
    val n = 3
    require(xs.nonEmpty)
    println(removeAt(n, xs))
    println(removeAt_(n, xs))
  }

  def removeAt[A](n: Int, xs: List[A]): (List[A], A) = {
    @scala.annotation.tailrec
    def rec(res: List[A], cnt: Int, curr: List[A]): List[A] = curr match {
      case Nil => res.reverse
      case _ if cnt == 0 => rec(res, cnt - 1, curr.tail)
      case h :: tail => rec(h :: res, cnt - 1, tail)
    }

    (rec(Nil, n, xs), xs(n))
  }

  def removeAt_[A](n: Int, xs: List[A]): (List[A], A) = (n, xs) match {
    case (0, h :: tail) => (tail, h) // exit case
    case (_, h :: tail) =>
      val (t, re) = removeAt_(n - 1, tail)
      (h :: t, re)
    case (_, Nil) => throw new IllegalArgumentException
  }

}
