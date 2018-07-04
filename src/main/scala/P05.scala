object P05 {
  def main(args: Array[String]): Unit = {
    lazy val target = List(1, 1, 2, 3, 5, 8)
    println(reverse(target))
    println(reverseRec(target))

    lazy val target01 = List(1, 2, 3, 4, 5)
    println(reverseFunc(target01))
    print(reverseTailRec(target01))
  }

  def reverse[A](l: List[A]): List[A] = l.reverse

  // O(n^2)
  def reverseRec[A](l: List[A]): List[A] = l match {
    case h :: tail => reverseRec(tail) ::: List(h)
    case Nil => Nil
  }

  def reverseFunc[A](lst: List[A]): List[A] = {
    lst.foldLeft(List[A]()) {
      (res, h) => h :: res
    }
  }

  def reverseTailRec[A](l: List[A]): List[A] = {
    def _rec(res: List[A], rem: List[A]): List[A] = rem match { // `rem` is remaining. current.
      case Nil => res
      case h :: tail => _rec(h :: res, tail)
    }

    _rec(Nil, l)
  }
}
