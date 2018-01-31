/**
  * Eliminate consecutive duplicates of list elements.
  * The order of the elements should not be changed.
  */
object P08 {
  def main(args: Array[String]): Unit = {
    val testList = Dummy.duplicatedList
    // println(compressRecursion(Dummy.duplicatedList))
    println(compressRecursionHelper(testList, testList.head))
  }

  def compressRecursion[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case lst => compressRecursionHelper(lst, lst.head)
 //   case
  }

  def compressRecursionHelper[A](l: List[A], h: A): List[A] = {
    h :: l.tail.filter(_ != h) // h は List の　head
    // l.head :: l.tail.filter(_ != l.head)
  }

  // TODO buffer を間に挟む方法. 試す.
}
