object P05 {
  def main(args: Array[String]): Unit = {
    println(reverse(Dummy.myList))
    println(reverseRecursion(Dummy.myList))

    print(reverseHelper(List(1, 2, 3, 4, 5)))
  }

  def reverse[A](l: List[A]): List[A] = l.reverse

  def reverseRecursion[A](l: List[A]): List[A] = l match {
    case h :: tail => reverseRecursion(tail) ::: List(h)
    case Nil => Nil
  }

  // :: 要素を List に連結
  // ::: List 連結

  def reverseHelper[A](l: List[A]): List[A] = {
    def _reverse(res: List[A], rem: List[A]): List[A] = rem match {
      case Nil => res
      case h :: tail => _reverse(h :: res, tail)
    }

    _reverse(Nil, l)
  }
}
