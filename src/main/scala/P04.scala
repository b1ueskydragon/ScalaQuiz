object P04 {
  def main(args: Array[String]): Unit = {
    print(length(Dummy.myList))
  }

  def length[A](l: List[A]): Int = l.length

  // List is immutable (a new list is created every time you do a modification)
  // Array is mutable
  // https://stackoverflow.com/questions/2712877/difference-between-array-and-list-in-scala
}
