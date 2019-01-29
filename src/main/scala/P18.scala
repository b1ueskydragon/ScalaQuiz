import scala.annotation.tailrec

object P18 {
  def main(args: Array[String]): Unit = {
    val xs = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    //val xs = Nil
    val (i, k) = (3, 7)
    //val (i, k) = (-2, 400)

    println(xs.slice(i, k)) // built-in
    println(slice_(i, k, xs))
    //println(slice__(i, k, xs))
    println(slice___(i, k, xs))
    println(slice____(i, k, xs))
    println(slice_____(i, k, xs))
  }

  def slice_[A](from: Int, to: Int, l: List[A]): List[A] = l.take(to).drop(from)

  //def slice__[A](from: Int, to: Int, l: List[A]): List[A] = l.drop(from).dropRight(to - from)

  def slice___[A](start: Int, end: Int, l: List[A]): List[A] = {
    @tailrec
    def rec(cursor: Int, current: List[A], rst: List[A]): List[A] = (cursor, current) match {
      case (_, Nil) => rst
      case (c, _) if end <= c => rst
      case (c, h :: tail) if start <= c => rec(c + 1, tail, rst ::: List(h))
      case (c, _ :: tail) => rec(c + 1, tail, rst)
    }

    rec(0, l, Nil)
  }

  def slice____[A](left: Int, right: Int, l: List[A]): List[A] = {
    @tailrec
    def rec(rst: List[A], left: Int, right: Int, l: List[A]): List[A] = l match {
      case _ :: tail if left > 0 => rec(rst, left - 1, right - 1, tail)
      case h :: tail if right > 0 => rec(rst ::: List(h), 0, right - 1, tail)
      case _ => rst // left, right is zero
    }

    rec(List(), left, right, l)
  }

  // TODO length is O(N), try foldl
  def slice_____[A](i: Int, k: Int, xs: List[A]): List[A] = {
    val xl = xs.length
    val t = if (k >= xl) xl else k
    xs.foldRight(xs) { (_, acc) =>
      val al = acc.length
      if (al > t) acc.init else if (al > t - i) acc.tail else acc
    }
  }

}
