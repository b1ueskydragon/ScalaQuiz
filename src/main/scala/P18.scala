object P18 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(slice(3, 7, target)) //List('d, 'e, 'f, 'g)
    println(sliceFunction(3, 7, target))
  }

  def slice[A](to: Int, from: Int, l: List[A]): List[A] = l.drop(to).dropRight(from - to)

  def sliceFunction[A](i: Int, k: Int, l: List[A]): List[A] = l.slice(i, k)

  // TODO Add recursive function.

  // ... if there are duplicated element, it will not work
  def sliceIndexFilter[A](to: Int, from: Int, l: List[A]): List[A] = l.filter(e => l.indexOf(e) >= to && l.indexOf(e) < from)
}
