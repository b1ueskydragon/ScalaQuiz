object P06 {
  def main(args: Array[String]): Unit = {
    val target = List(1, 2, 3, 2, 1)
    println(isPalindrome(target))
    println(isPalindromeRecursion(target))
  }

  def isPalindrome[A](l: List[A]): Boolean = l.reverse == l

  def isPalindromeRecursion[A](l: List[A]): Boolean = l match {
    case Nil => true
    case List(a) => true
    case list => list.head == list.last && isPalindromeRecursion(list.tail.init)
  }
}
