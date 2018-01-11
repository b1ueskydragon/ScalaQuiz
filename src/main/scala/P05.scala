object P05 {
  def main(args: Array[String]): Unit = {
    println(reverse(Dummy.myList))
    println(reverseRecursion(Dummy.myList))
  }

  def reverse[A](l: List[A]): List[A] = l.reverse

  // ::: の使い方？
  def reverseRecursion[A](l: List[A]): List[A] = l match {
    case h :: tail => reverseRecursion(tail) ::: List(h)
    case Nil => Nil
  }
}
