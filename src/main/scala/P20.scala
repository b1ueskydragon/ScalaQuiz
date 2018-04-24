object P20 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd)
    println(removeAt(3, target)) // (List('a, 'c, 'd),'b)
    println(removeAtAnother(3, target))
  }

  def removeAt[A](n: Int, l: List[A]): (List[A], A) = {
    def _recursion(sub: List[A], cnt: Int, l: List[A]): List[A] = l match {
      case Nil => sub
      case _ if cnt == 0 => _recursion(sub, cnt - 1, l.tail)
      case _ => _recursion(sub :+ l.head, cnt - 1, l.tail)
    }

    (_recursion(Nil, n, l), l(n))
  }

  def removeAtAnother[A](n: Int, l: List[A]): (List[A], A) = {
    (n, l) match {
      case (0, h :: tail) => (tail, h) // exit case
      case (_, h :: tail) =>
        val (t, re) = removeAtAnother(n - 1, tail)
        (h :: t, re)
    }
  }
}
