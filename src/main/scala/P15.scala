import scala.annotation.tailrec

object P15 {
  def main(args: Array[String]): Unit = {
    val n = 3
    val xs = List('a, 'b, 'c, 'c, 'd)
    println(duplicateN(n, xs))
    println(duplicateN_(n, xs))
  }

  def duplicateN[A](n: Int, l: List[A]): List[A] = {
    val k = n

    @tailrec
    def rec(n: Int, acc: List[A], l: List[A]): List[A] = l match {
      case Nil => acc
      case xs if n > 1 => rec(n - 1, acc ::: List(xs.head), l)
      case h :: tail => rec(k, acc ::: List(h), tail)
    }

    rec(k, List(), l)
  }

  def duplicateN_[A](n: Int, xs: List[A]): List[A] = xs.flatMap(List.fill(n)(_))
}
