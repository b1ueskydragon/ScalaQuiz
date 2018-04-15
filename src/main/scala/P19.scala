object P19 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(rotate(3, target)) //  List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    //println(rotate(-2, target)) //  List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }

  def rotate[A](i: Int, l: List[A]): List[A] = {
    def _rotate(cnt: Int, rst: List[A], tmp: List[A], ori: List[A]): List[A] = (cnt, ori) match {
      case (_, List()) => rst ::: tmp
      case (c, h :: tail) if c > 0 => _rotate(cnt - 1, rst, tmp ::: List(h), tail)
      case (c, h :: tail) if c == 0 => _rotate(0, rst ::: List(h), tmp, tail)
    }

    _rotate(i, List(), List(), l)
  }
}
