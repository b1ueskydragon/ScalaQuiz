object P06 {
  def main(args: Array[String]): Unit = {
    val target = List(1, 2, 4, 8, 16, 8, 4, 2, 1)
    println(isPalindrome(target))
    println(isPalindrome_(target))
  }

  def isPalindrome[A](l: List[A]): Boolean = l.reverse == l

  def isPalindrome_[A](l: List[A]): Boolean = l match {
    case Nil => true
    case List(_) => true
    case list => list.head == list.last && isPalindrome_(list.tail.init)
  }
}
