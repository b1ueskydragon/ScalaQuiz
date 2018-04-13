object P16 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(drop(3, target))
    println(dropTailRecursion(3, target))
  }

  def drop[A](n: Int, l: List[A]): List[A] = l.flatMap {
    case e if l.indexOf(e) % n == n - 1 => List() ::: List() // 毎回新しく生成される
    case e => List() ::: List(e) // same as above
  }

  // 再帰中 drop したい head だけ result にくっつけない.
  def dropTailRecursion[A](n: Int, l: List[A]): List[A] = {
    def _prepend(cnt: Int, current: List[A], rst: List[A]): List[A] = (cnt, current) match {
      case (_, Nil) => rst.reverse
      case (1, _ :: tail) => _prepend(n, tail, rst)
      case (_, h :: tail) => _prepend(cnt - 1, tail, h :: rst)
    }

    _prepend(n, l, List())
  }
}

