object P16 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(drop(3, target))
    println(dropTailRecursion(3, target))
    println(dropRecursion(3, target))
    println(dropOneLiner(3, target))
    println(dropAnother(3, target))

  }

  def dropAnother[A](n: Int, l: List[A]): List[A] = {
    def _dropOrAppend(res: List[A], cnt: Int, l: List[A]): List[A] = l match {
      case Nil => res
      case h :: tail if cnt < (n - 1) => _dropOrAppend(res ::: List(h), cnt + 1, tail)
      case _ => _dropOrAppend(res, 0, l.tail) // reset counter
    }

    _dropOrAppend(List(), 0, l)
  }

  def drop[A](n: Int, l: List[A]): List[A] = l.flatMap {
    case e if l.indexOf(e) % n == n - 1 => List() ::: List() // 毎回新しく生成される
    case e => List() ::: List(e) // same as above
  }

  // Tail Recursive.
  // 再帰中 drop したい head だけ result にくっつけない.
  def dropTailRecursion[A](n: Int, l: List[A]): List[A] = {
    def _prepend(cnt: Int, current: List[A], rst: List[A]): List[A] = (cnt, current) match {
      case (_, Nil) => rst.reverse
      case (1, _ :: tail) => _prepend(n, tail, rst)
      case (_, h :: tail) => _prepend(cnt - 1, tail, h :: rst)
    }

    _prepend(n, l, List())
  }

  def dropRecursion[A](n: Int, l: List[A]): List[A] = {
    def _append(cnt: Int, current: List[A]): List[A] = (cnt, current) match {
      case (_, Nil) => Nil
      case (1, _ :: tail) => _append(n, tail)
      case (_, h :: tail) => h :: _append(cnt - 1, tail)
    }

    _append(n, l)
  }

  // make functional.
  // [[ zipWithIndex ]] python enumerate のような感じで index と value マッピング.
  def dropOneLiner[A](n: Int, l: List[A]): List[A] = l.zipWithIndex filter (e => (e._2 + 1) % n != 0) map (_._1)
}

