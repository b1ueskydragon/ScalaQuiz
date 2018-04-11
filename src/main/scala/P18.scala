object P18 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(sliceDrop(3, 7, target))
    println(sliceFunction(3, 7, target))
    println(sliceTake(3, 7, target))
    println(sliceRecursion(3, 7, target))
    println(sliceRecursionTail(3, 7, target))
  }

  def sliceDrop[A](to: Int, from: Int, l: List[A]): List[A] = l.drop(to).dropRight(from - to)

  def sliceFunction[A](i: Int, k: Int, l: List[A]): List[A] = l.slice(i, k)

  def sliceTake[A](to: Int, from: Int, l: List[A]): List[A] = l.take(from).drop(to)

  // start and end point is fixed.
  def sliceRecursion[A](start: Int, end: Int, l: List[A]): List[A] = {
    def _recursion(cursor: Int, current: List[A], rst: List[A]): List[A] = (cursor, current) match {
      case (_, Nil) => rst // 何もしない
      case (c, h :: tail) if end <= c => println(h + " : go out of range"); rst // 何もしない
      case (c, h :: tail) if start <= c => println(h + " : enter in range"); _recursion(c + 1, tail, rst ::: List(h))
      case (c, h :: tail) => println(h + " : heading to start point"); _recursion(c + 1, tail, rst)
    }

    _recursion(0, l, Nil)
  }

  // same as above but tail recursive.
  def sliceRecursionTail[A](start: Int, end: Int, l: List[A]): List[A] = {
    def _recursion(cursor: Int, current: List[A], rst: List[A]): List[A] = (cursor, current) match {
      case (_, Nil) => rst.reverse
      case (c, h :: tail) if end <= c => rst.reverse
      case (c, h :: tail) if start <= c => _recursion(c + 1, tail, h :: rst)
      case (c, h :: tail) => _recursion(c + 1, tail, rst)
    }

    _recursion(0, l, Nil)
  }

  // ... if there are duplicated element, it won't work
  def sliceIndexFilter[A](to: Int, from: Int, l: List[A]): List[A] = l.filter(e => l.indexOf(e) >= to && l.indexOf(e) < from)
}
