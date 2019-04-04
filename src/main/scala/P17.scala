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
    println(split____(n, xs))
    println(split_____(n, xs))
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

  // get length takes O(N)
  def split____[A](n: Int, xs: List[A]): (List[A], List[A]) =
    xs.foldRight((xs.length, (List[A](), List[A]()))) { (x, acc) =>
      if (acc._1 > n) (acc._1 - 1, (acc._2._1, x :: acc._2._2))
      else (n, (x :: acc._2._1, acc._2._2))
    }._2

  // without get length but list concat with foldLeft is slow
  def split_____[A](n: Int, xs: List[A]): (List[A], List[A]) =
    xs.foldLeft(0, (List[A](), List[A]())) { (acc, x) =>
      acc match {
        case (i, (_, r)) if i < n => (i + 1, (acc._2._1 ::: List(x), r))
        case (i, (l, _)) => (i + 1, (l, acc._2._2 ::: List(x)))
      }
    }._2

}
