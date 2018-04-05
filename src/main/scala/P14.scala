object P14 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'c, 'd)
    println(duplicate(target))
  }
  def duplicate[A](list: List[A]): List[A] = list.flatMap (e => List(e, e))
}
