object P02 {
  def main(args: Array[String]) {
    print(penultimate(Dummy.myList))
  }

  def penultimate[A](l: List[A]): A = {
    if (l.isEmpty) throw new NoSuchElementException
    l.init.last
  }
}
