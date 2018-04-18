object P19 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(rotateProtoType(3, target)) //  List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    println(rotateProtoType(-2, target)) //  List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

    println(rotateRefactor(3, target))
    println(rotateRefactor(-2, target))

    println(rotateTakeDrop(3, target))
    println(rotateTakeDrop(-2, target))

    println(rotateProtoType(14, target)) // out of length return itself
    println(rotate(14, target)) // and fixed

    println(rotateRefactor(14, target))
    println(rotateBound(14, target))
  }

  // [[ scala.Array.:+ ]] A copy of this array with an element appended.
  // tail :+ h == tail ::: List(h)

  def rotateProtoType[A](i: Int, l: List[A]): List[A] = {
    def _rotate(cnt: Int, rst: List[A], tmp: List[A], ori: List[A]): List[A] = (cnt, ori) match {
      // i is negative
      case (_, List()) if i < 0 => tmp.reverse ::: rst.reverse
      case (c, lst) if c < 0 => _rotate(-c, rst, tmp, lst.reverse)

      // i is positive
      case (_, List()) => rst ::: tmp
      case (c, h :: tail) if c > 0 => _rotate(cnt - 1, rst, tmp :+ h, tail)
      case (c, h :: tail) if c == 0 => _rotate(0, rst :+ h, tmp, tail)
    }

    _rotate(i, List(), List(), l)
  }

  def rotate[A](cnt: Int, current: List[A]): List[A] = (cnt, current) match {
    case (c, h :: tail) if c > 0 => rotate(c - 1, tail ::: List(h))
    case (c, lst) if c < 0 => rotate(c + 1, lst.last :: lst.init)
    case (0, _) => current
  }

  def rotateRefactor[A](i: Int, l: List[A]): List[A] = l match {
    case h :: tail if i > 0 => rotateRefactor(i - 1, tail :+ h)
    case init :+ last if i < 0 => rotateRefactor(i + 1, last :: init)
    case _ => l
  }

  def rotateBound[A](i: Int, l: List[A]): List[A] = {
    val bound = if (l.isEmpty) 0 else i % l.length
    if (bound < 0) rotateBound(bound + l.length, l)
    else (l drop bound) ::: (l take bound)
  }

  // take and drop
  def rotateTakeDrop[A](i: Int, l: List[A]): Any = {
    if (i >= 0) (l drop i) ::: (l take i)
    else (l drop (l.length + i)) ::: (l take l.length + i)
  }
}
