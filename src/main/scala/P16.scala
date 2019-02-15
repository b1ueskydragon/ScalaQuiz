import scala.annotation.tailrec

/** Drop every Nth element from a list. */
object P16 {
  def main(args: Array[String]): Unit = {
    // val n = 0
    val n = 3
    val xs = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'c, 'k)
    println(drop(n, xs))
    println(drop_(n, xs))
    println(drop__(n, xs))
    println(drop___(n, xs))
    println(drop____(n, xs))
  }

  def drop[A](n: Int, xs: List[A]): List[A] =
    if (n == 0) xs else xs.grouped(n).flatMap(_.take(n - 1)).toList

  // concat List (:::) is expensive
  def drop_[A](n: Int, xs: List[A]): List[A] = {
    @tailrec
    def rec(cnt: Int, curr: List[A], rst: List[A]): List[A] = (cnt, curr) match {
      case (_, Nil) => rst
      case (1, _ :: tail) => rec(n, tail, rst)
      case (_, h :: tail) => rec(cnt - 1, tail, rst ::: List(h))
    }

    rec(n, xs, List())
  }

  // same logic as above. stack will get deeper if xs has large length.
  // which one is more efficient?
  def drop__[A](n: Int, xs: List[A]): List[A] = {
    def acc(cnt: Int, curr: List[A]): List[A] = (cnt, curr) match {
      case (_, Nil) => Nil
      case (1, _ :: tail) => acc(n, tail)
      case (_, h :: tail) => h :: acc(cnt - 1, tail)
    }

    acc(n, xs)
  }

  // zipWithIndex, mapping index and value
  def drop___[A](n: Int, xs: List[A]): List[A] =
    if (n == 0) xs else xs.zipWithIndex.filter(e => (e._2 + 1) % n != 0).map(_._1)

  def drop____[A](n: Int, xs: List[A]): List[A] = {
    @tailrec
    def rec(res: List[A], cnt: Int, l: List[A]): List[A] = l match {
      case Nil => res
      case h :: tail if cnt < (n - 1) => rec(res ::: List(h), cnt + 1, tail)
      case _ => rec(res, 0, l.tail) // reset counter
    }

    rec(List(), 0, xs)
  }

}

