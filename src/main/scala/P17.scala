import scala.annotation.tailrec

object P17 {
  def main(args: Array[String]): Unit = {
    val n = 3
    //val n = 0
    val xs = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

    println(xs.splitAt(n))
    println(split(n, xs))
    println(split_(n, xs))
    println(split__(n, xs))
    println(split___(n, xs))
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

  // simply one
  def split__[A](n: Int, xs: List[A]): (List[A], List[A]) = (xs.take(n), xs.drop(n))

  def split___[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    val size = xs.length - n
    val right = xs.foldRight(List[A]())((x, acc) => if (acc.length < size) x :: acc else acc)

    (xs diff right, right)
  }

}
