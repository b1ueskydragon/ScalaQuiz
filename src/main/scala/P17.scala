object P17 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(split(3, target))
    println(splitRecursion(3, List(), List(), target))
  }

  def split[A](n: Int, l: List[A]): (List[A], List[A]) = l.span(e => l.indexOf(e) < n)

  def splitRecursion[A](n: Int, left: List[A], right: List[A], l: List[A]): (List[A], List[A]) = l match {
    case Nil => (left, right)
    case h :: tail if left.length < n => splitRecursion(n, left ::: List(h), right, tail)
    case h :: tail => splitRecursion(n, left, right ::: List(h), tail)
  }
}
