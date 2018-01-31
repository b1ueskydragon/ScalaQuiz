object P01 {
  def main(args: Array[String]): Unit = {
    println(last(Dummy.myList))
    println(lastRecursion(Dummy.myList))
  }

  def last[A](l: List[A]): A = l.last // last() already throws the NoSuchElementException

  def lastRecursion[A](l: List[A]): A = l match {
    case h :: Nil => h // one head element and a tail formed by nothing (input list has only one element)
    case _ :: tail => lastRecursion(tail) // takes the tail of the list and calls itself on the remaining values (the first element is not even stored, it is ignored using the _ wildcard)
    case _ => throw new NoSuchElementException // when the list is empty
  }

  // match case 文でできるだけのパターンが網羅できるように工夫する.
  // h :: tail => リターンで h を使うために変数宣言
  // _ :: tail => リターンで 別にヘッドを使うことはない
}
