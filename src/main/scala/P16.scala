object P16 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'zz, 'z2)
    println(_drop(List(), 3, target))
    // List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  def _drop[A](result: List[A], n: Int, l: List[A]): List[A] = l.flatMap {
    case e if l.indexOf(e) % n == n - 1 => result ::: List()
    case e => result ::: List(e)
  }

  // よくない例
  def packByN[A](res: List[A], tmp: List[A], n: Int, l: List[A]): List[Any] = l.map {
    case e if tmp.length < n =>
      println("1" + e)
      tmp ::: List(e) // なぜならここで毎回新しい tmp 作成される
    case e if tmp.length == n =>
      println("2" + e)
      res ::: tmp
  }
}

