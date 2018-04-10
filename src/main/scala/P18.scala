object P18 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(slice(List(), 3, 7, target)) //List('d, 'e, 'f, 'g)
  }

  def slice[A](result: List[A], to: Int, from: Int, l: List[A]): List[A] = l match {
    case Nil => result
    case h :: tail if tail.length > from => slice(result ::: List(h), to, from - 1, tail)
  }
}
