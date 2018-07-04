object P07 {
  def main(args: Array[String]): Unit = {
    val target = List(List(1, 2), 3, List(4, List(5, 6)))
    println(flatten(target))
  }

  def flatten[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case (h: List[A]) :: tail => flatten(h) ::: flatten(tail) // l の head が A 型の List なのか? (recursive case. どこかのタイミングで最小単位要素となる)
    case (h: A) :: tail => h :: flatten(tail)                 // head が A 型の最小単位の要素なのか? (base case)
  }
}
