import scala.annotation.tailrec

object P12 {
  def main(args: Array[String]): Unit = {
    val given = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    println(decode(given))
    println(decode_(given))
    println(decode__(given))
    println(decode___(given))
  }

  def decode[A](l: List[(Int, A)]): List[A] = {
    @tailrec
    def rec(res: List[A], xs: List[(Int, A)]): List[A] = xs match {
      case Nil => res
      case h :: tail if h._1 > 1 => rec(res ::: List(h._2), (h._1 - 1, h._2) :: tail)
      case h :: tail => rec(res ::: List(h._2), tail)
    }

    rec(List(), l)
  }

  // reverse is O(N)
  // refactor above but not simple
  def decode_[A](l: List[(Int, A)]): List[A] = {
    // TODO: with lazy eval with stream
    // TODO: replace to List.fill(r)(e)
    def replicate(r: Int, e: A): List[A] = {
      @tailrec
      def rec(res: List[A], r: Int, e: A): List[A] =
        if (r == 1) e :: res else rec(e :: res, r - 1, e)

      rec(List(), r, e)
    }

    @tailrec
    def rec(res: List[A], xs: List[(Int, A)]): List[A] = xs match {
      case Nil => res.reverse
      case h :: tail =>
        if (h._1 > 1) rec(replicate(h._1, h._2) ::: res, tail)
        else rec(h._2 :: res, tail)
    }

    rec(List(), l)
  }

  // refactor above with foldr. simple but I don't like ::: concatenation
  def decode__[A](xs: List[(Int, A)]): List[A] = xs.foldRight(List[A]()) { (x, acc) =>
    if (x._1 > 1) List.fill(x._1)(x._2) ::: acc else x._2 :: acc
  }

  // do more simply that above with flatten
  def decode___[A](xs: List[(Int, A)]): List[A] = xs.flatMap(x => List.fill(x._1)(x._2))

}
