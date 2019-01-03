object P06 {
  def main(args: Array[String]): Unit = {
    val target = List(1, 2, 4, 8, 16, 8, 4, 2, 1)
    println(isPalindrome(target))
    println(isPalindrome_(target))
    println(isPalindrome__(target))
    println(isPalindrome___(target))
  }

  def isPalindrome[A](l: List[A]): Boolean = l.reverse == l

  def isPalindrome_[A](l: List[A]): Boolean = l match {
    case Nil => true
    case List(_) => true
    case list => list.head == list.last && isPalindrome_(list.tail.init)
  }

  def isPalindrome__[A](l: List[A]): Boolean = {
    val half = l.length / 2
    l.take(half).reverse == l.drop(half + l.length % 2)
  }

  def isPalindrome___[A](l: List[A]): Boolean = {
    val t = l.splitAt(l.length / 2)
    t._1.reverse == t._2.drop(l.length % 2)
  }

}
