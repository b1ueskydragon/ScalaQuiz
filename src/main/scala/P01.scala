object P01 {
  def main(args: Array[String]): Unit = {
    print(last(Dummy.myList))
  }

  def last[A](l: List[A]): A = {
  if(l.isEmpty) throw new NoSuchElementException
    l.last
  }
}
