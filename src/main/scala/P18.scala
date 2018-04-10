object P18 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(sliceDrop(3, 7, target)) //List('d, 'e, 'f, 'g)
    println(sliceFunction(3, 7, target))
    println(sliceTake(3, 7, target))
    println(sliceRecursion(3, 7, target))
  }

  def sliceDrop[A](to: Int, from: Int, l: List[A]): List[A] = l.drop(to).dropRight(from - to)

  def sliceFunction[A](i: Int, k: Int, l: List[A]): List[A] = l.slice(i, k)

  def sliceTake[A](to: Int, from: Int, l: List[A]): List[A] = l.take(from).drop(to)

  def sliceRecursion[A](start: Int, end: Int, l: List[A]): List[A] = (start, end, l) match {
    case (0, 0, rst) => rst
    case (0, e, rst) => sliceRecursion(0, e - 1, l.init)
    case (s, e, h :: tail) => sliceRecursion(s - 1, e, tail)
  }

  // ... if there are duplicated element, it will not work
  def sliceIndexFilter[A](to: Int, from: Int, l: List[A]): List[A] = l.filter(e => l.indexOf(e) >= to && l.indexOf(e) < from)
}
