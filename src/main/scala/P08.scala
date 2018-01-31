/**
  * Eliminate consecutive duplicates of list elements.
  * The order of the elements should not be changed.
  */
object P08 {
  def main(args: Array[String]): Unit = {
    println(compressRecursion(Dummy.duplicatedList))
  }

  // TODO ただの重複除去でよかった??
  def compressRecursion[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: List() => List(h)
    case h :: tail if h == tail.head => compressRecursion(tail)
    case h :: tail => h :: compressRecursion(tail)
  }

  def compressRecursionHelper[A](l: List[A], h: A): List[A] = {
    h :: l.tail.filter(_ != h) // h は List の　head
  }

  // TODO buffer を間に挟む方法. 試す.
}
