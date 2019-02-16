import scala.annotation.tailrec

object P17 {
  def main(args: Array[String]): Unit = {
    val xs = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    //val xs = Nil
    val n = 3
    //val n = 0

    println(xs.splitAt(n))
    println(split(n, xs))
    println(split_(n, xs))
    println(split__(n, xs))
    println(split___(n, xs))
  }

  def split[A](n: Int, xs: List[A]): (List[A], List[A]) = xs.span(xs.indexOf(_) < n)

  def split_[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    @tailrec
    def rec(n: Int, xs: List[A], left: List[A], right: List[A]): (List[A], List[A]) = xs match {
      case _ if n <= 0 || xs.isEmpty => (left.reverse, xs)
      case h :: tail => rec(n - 1, tail, h :: left, right)
    }

    rec(n, xs, List(), List())
  }

  // simply one
  def split__[A](n: Int, xs: List[A]): (List[A], List[A]) = (xs.take(n), xs.drop(n))

  def split___[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    val size = xs.length - n
    val right = xs.foldRight(List[A]())((x, acc) => if (acc.length < size) x :: acc else acc)

    (xs diff right, right)
  }

}
