import scala.annotation.tailrec

// Pack consecutive duplicates of list elements into sublists.
object P09 {
  def main(args: Array[String]): Unit = {
    lazy val given = List('a, 'a, 'a, 'b, 'b, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(pack_(given))
    println(pack(given))
  }

  def pack_[A](l: List[A]): List[List[A]] = {
    @tailrec
    def rec(xs: List[A], stack: List[A], acc: List[List[A]]): List[List[A]] =
      (xs, stack, acc) match {
        case (Nil, sub, res) => sub :: res
        case (h :: tail, sub, res) =>
          if (sub.isEmpty || h == sub.head) rec(tail, h :: sub, res)
          else rec(tail, List(h), sub :: res)
      }

    rec(l, List(), List()).reverse
  }

  def pack[A](l: List[A]): List[List[A]] = {
    /**
      * @param res outer list (initial status is Nil)
      * @param xs  given list unpacked yet (elements of outer list)
      * @return res
      */
    @tailrec
    def rec(res: List[List[A]], xs: List[A]): List[List[A]] = xs match {
      // exit case: return outer list if lookup is end
      case Nil => res
      // standard if case: sublist is not exist yet, so append one
      case h :: tail if res.isEmpty || res.last.head != h => rec(res ::: List(List(h)), tail)
      // standard else case: there is a sublist, so put it in
      case h :: tail => rec(res.init ::: List(res.last ::: List(h)), tail)
    }

    rec(List(), l)
  }

}
