object P14 {
  def main(args: Array[String]): Unit = {
    val xs = List('a, 'b, 'c, 'c, 'd)
    println(duplicate(xs))
    println(duplicate_(xs))
    println(duplicate__(xs))
  }

  def duplicate[A](xs: List[A]): List[A] = xs.flatMap(e => List(e, e))

  def duplicate_[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: tail => h :: h :: duplicate_(tail)
  }

  def duplicate__[A](xs: List[A]): List[A] = xs.foldRight(List[A]())((x, acc) => x :: x :: acc)
  
}
