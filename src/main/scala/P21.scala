object P21 {
  def main(args: Array[String]): Unit = {
    val origin = List('a, 'b, 'c, 'd)
    println(insertAt('new, 1, origin)) // List('a, 'new, 'b, 'c, 'd)
    println(insertAtFunction('new, 1, origin))
  }

  def insertAt[A](newEl: A, place: Int, l: List[A]): List[A] = {
    def recursion(p: Int, rst: List[A], ori: List[A]): List[A] = {
      if (place >= l.length)
        l :+ newEl
      //throw new NoSuchElementException

      else ori match {
        case Nil => rst
        case h :: tail if p < place => recursion(p + 1, rst :+ h, tail)
        case _ => recursion(p + 1, rst ++ List(newEl) ++ ori, Nil)
      }
    }

    recursion(0, Nil, l)
  }

  // splitAt returns tuple.
  def insertAtFunction[A](e: A, p: Int, l: List[A]): List[A] = l.splitAt(p) match {
    case (pre, post) => pre ::: e :: post
  }
}
