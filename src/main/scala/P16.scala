object P16 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(drop(3, target))
  }

  def drop[A](n: Int, l: List[A]): List[A] = l.flatMap {
    case e if l.indexOf(e) % n == n - 1 => List() ::: List() // 毎回新しく生成される
    case e => List() ::: List(e) // same as above
  }
}

