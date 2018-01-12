object P05 {
  def main(args: Array[String]): Unit = {
    println(reverse(Dummy.myList))
    println(reverseRecursion(Dummy.myList))
  }

  def reverse[A](l: List[A]): List[A] = l.reverse

  def reverseRecursion[A](l: List[A]): List[A] = l match {
    case h :: tail => reverseRecursion(tail) ::: List(h)
    case Nil => Nil
  }
  // :: 要素を List に連結
  // ::: List 連結
}
