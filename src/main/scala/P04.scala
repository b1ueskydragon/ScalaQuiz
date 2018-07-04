object P04 {
  def main(args: Array[String]): Unit = {
    val target = List(1, 1, 2, 3, 5, 8)

    println(length(target))
    println(lengthRecursive(target))
  }

  def length[A](l: List[A]): Int = l.length

  // List is immutable (a new list is created every time you do a modification)
  // Array is mutable
  // https://stackoverflow.com/questions/2712877/difference-between-array-and-list-in-scala

  def lengthRecursive[A](l: List[A]): Int = l match {
    case Nil => 0 // Nil は空のリスト
    case h :: tail =>
      //println(h)
      1 + lengthRecursive(tail)
    // :: は既存の List に要素を連結するメソッド、元となる空 List(Nil)が必要
    // tail は List の戦闘要素を取り除いた List
  }
}
