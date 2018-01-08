object P03 {
  def main(args: Array[String]): Unit = {
    print(nth(9, Dummy.myList))
  }

  def nth[A](k: Int, l: List[A]): A = {
    try l(k)
    catch {
      case e: IndexOutOfBoundsException => throw new NoSuchElementException
    }
  }
}
