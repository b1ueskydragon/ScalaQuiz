import scala.annotation.tailrec

/** Pack consecutive duplicates of list elements into sublists. */
object P09 {
  def main(args: Array[String]): Unit = {
    lazy val given = List('a, 'a, 'a, 'b, 'b, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(pack_(given))
    println(pack(given))
    println(pack__(given))
    println(pack___(given))
    println(pack____(given))
    println(pack_____(given))
    println(pack______(given))
  }

  def pack_[A](l: List[A]): List[List[A]] = {
    @tailrec
    def rec(xs: List[A], stack: List[A], acc: List[List[A]]): List[List[A]] =
      (xs, stack, acc) match {
        case (Nil, sub, res) => res ::: List(sub)
        case (h :: tail, sub, res) =>
          if (sub.isEmpty || h == sub.head) rec(tail, h :: sub, res)
          else rec(tail, List(h), res ::: List(sub))
      }

    rec(l, List(), List())
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

  def pack__[A](l: List[A]): List[List[A]] = {
    @tailrec
    def rec(acc: List[List[A]], xs: List[A]): List[List[A]] =
      if (xs.isEmpty) acc else rec(acc ::: List(xs.takeWhile(_ == xs.head)), xs.dropWhile(_ == xs.head))

    rec(List(), l)
  }

  def pack___[A](l: List[A]): List[List[A]] = {
    @tailrec
    def rec(res: List[List[A]], xs: List[A]): List[List[A]] = {
      if (xs.isEmpty) res
      else {
        val s = xs.span(_ == xs.head)
        rec(res ::: List(s._1), s._2)
      }
    }

    rec(List(), l)
  }

  def pack____[A](l: List[A]): List[List[A]] = l.foldRight(List[List[A]]()) { (x, acc) =>
    acc match {
      case h :: tail if h.isEmpty || x == h.head => (x :: h) :: tail
      case _ => List(x) :: acc // includes case Nil
    }
  }

  def pack_____[A](l: List[A]): List[List[A]] = {
    val (acc, rest) = l.splitAt(l.indexWhere(_ != l.head))
    if (acc.isEmpty) List(l)
    else acc :: pack_____(rest)
  }

  def pack______[A](l: List[A]): List[List[A]] = {
    @tailrec
    def rec(res: List[List[A]], l: List[A]): List[List[A]] = {
      val (acc, rest) = l.splitAt(l.indexWhere(_ != l.head))
      if (acc.isEmpty) res ::: List(l)
      else rec(res ::: List(acc), rest)
    }

    rec(Nil, l)
  }

}
