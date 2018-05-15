object P05 {
  def main(args: Array[String]): Unit = {
    println(reverse(Dummy.myList))
    println(reverseRec(Dummy.myList))

    print(reverseTailRec(List(1, 2, 3, 4, 5)))
  }

  def reverse[A](l: List[A]): List[A] = l.reverse

  // O(n^2)
  def reverseRec[A](l: List[A]): List[A] = l match {
    case h :: tail => reverseRec(tail) ::: List(h)
    case Nil => Nil
  }

  def reverseTailRec[A](l: List[A]): List[A] = {
    def _rec(res: List[A], rem: List[A]): List[A] = rem match { // `rem` is remaining. current.
      case Nil => res
      case h :: tail => _rec(h :: res, tail)
    }

    _rec(Nil, l)
  }
}
