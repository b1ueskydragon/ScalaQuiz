object P03 {
  def main(args: Array[String]): Unit = {
    val target = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    val k = 9
    println(findKth(k, target))
    println(findKth_(k, target))
    println(findKth__(k, target))
    println(findKth___(k, target))
    println(findKth____(k, target))
  }

  def findKth[A](k: Int, l: List[A]): A = {
    try l(k)
    catch {
      case _: IndexOutOfBoundsException => throw new NoSuchElementException
    }
  }

  def findKth_[A](k: Int, l: List[A]): A = k match {
    case 0 => l.head // exit case.
    case _ if k > 0 => findKth_(k - 1, l.tail) // standard case.
    case _ => throw new NoSuchElementException // covers the empty list case
  }

  def findKth__[A](k: Int, l: List[A]): A = l.drop(k).head

  def findKth___[A](k: Int, l: List[A]): A = l.take(k + 1).last

  def findKth____[A](k: Int, l: List[A]): A = l.zip((0 to k).toList).last._1

}
