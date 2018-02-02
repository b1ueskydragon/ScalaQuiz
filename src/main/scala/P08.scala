/**
  * Eliminate consecutive duplicates of list elements.
  * The order of the elements should not be changed.
  */
object P08 {
  def main(args: Array[String]): Unit = {
    println(compressRecursion(Dummy.duplicatedList))
  }

  def compressRecursion[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: List() => List(h)                                 // exit case. 一つしかない場合そのまま返す
    case h :: tail if h == tail.head => compressRecursion(tail) // standard case 1. head と tail.head が同じなら tail.head を持ってきて 元の head をとる
    case h :: tail => h :: compressRecursion(tail)              // standard case 2. head と tail.head が異なる場合, 何もしない (そのままくっつける)
  }

  //def compressRecursionHelper[A](l: List[A]): List[A] = l.head :: l.tail.filter(_ != l.head) // h は List の　head
  // TODO buffer を間に挟む方法. 試す.
}
