object P07 {
  def main(args: Array[String]): Unit = {
    println(Dummy.nestedList)
    println(flatten(Dummy.nestedList))
  }

  def flatten[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case (h: List[A]) :: tail => flatten(h) ::: flatten(tail) // l の head が A 型の List なのか? (recursive case. どこかのタイミングで最小単位要素となる)
    case (h: A) :: tail => List(h) ::: flatten(tail)          // head が A 型の最小単位の要素なのか? (base case)
  }
}
