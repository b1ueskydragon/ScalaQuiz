/**
  * Eliminate consecutive duplicates of list elements.
  * The order of the elements should not be changed.
  */
object P08 {
  def prototype01[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: tail => h :: prototype01(tail)
    case List(e0, e1) if e0 == e1 => List(e0)
  }

  def prototype02[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h::tail if h == tail.head => h :: tail.tail
  }
}
