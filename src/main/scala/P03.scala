object P03 {
  def main(args: Array[String]): Unit = {
    val target = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    val k = 9
    println(findKth(k, target))
    println(findKthRecursion(k, target))
  }

  def findKth[A](k: Int, l: List[A]): A = {
    try l(k)
    catch {
      case e: IndexOutOfBoundsException => throw new NoSuchElementException
    }
  }

  def findKthRecursion[A](k: Int, l: List[A]): A = k match {
    case 0 => l.head // exit case.
    case k if k > 0 => findKthRecursion(k - 1, l.tail) // standard case.

    case _ => throw new NoSuchElementException // else (covers the empty list case).
  }
}
