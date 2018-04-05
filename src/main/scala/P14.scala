object P14 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'c, 'd)
    println(duplicate(target))

    //  List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }

  def duplicate[A](list: List[A]): List[Any] = list.flatMap {
    e => List(e, e)
  }
}
