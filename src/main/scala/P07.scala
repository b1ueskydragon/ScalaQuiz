object P07 {
  def main(args: Array[String]): Unit = {
    val given = List(List(1, 2), 3, List(4, List(5, 6)))
    println(flatten(given))
    println(flatten_(given))
  }

  def flatten(l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case (h: List[_]) :: tail => flatten(h) ::: flatten(tail)
    case h :: tail => h :: flatten(tail)
  }

  def flatten_(l: List[Any]): List[Any] = l.flatMap {
    case xs: List[_] => flatten_(xs)
    case x => List(x)
  }
}
