/**
  * Eliminate consecutive duplicates of list elements.
  * The order of the elements should not be changed.
  */
object P08 {
  def main(args: Array[String]): Unit = {
  //  println(compressRecursion(Dummy.duplicatedList))
    println(compressRecursionHelper(Dummy.duplicatedList))
  }

  def compressRecursionHelper[A](l: List[A]): List[A] = l match {
    case h :: tail => h :: tail.filter(_ != h)
  }
}
