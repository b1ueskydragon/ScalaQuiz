import scala.annotation.tailrec

object P17 {
  def main(args: Array[String]): Unit = {
    val xs = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(split(3, xs))
    println(split_(3, xs))
  }

  def split[A](n: Int, xs: List[A]): (List[A], List[A]) = xs.span(xs.indexOf(_) < n)

  def split_[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    @tailrec
    def rec(n: Int, left: List[A], right: List[A], l: List[A]): (List[A], List[A]) = l match {
      case Nil => (left, right)
      case h :: tail =>
        if (left.length < n) rec(n, left ::: List(h), right, tail)
        else rec(n, left, right ::: List(h), tail)
    }

    rec(n, List(), List(), xs)
  }

  // TODO add case
}
