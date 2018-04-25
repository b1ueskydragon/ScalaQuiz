import scala.util.Random

object P23 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)
    println(randomSelectRec(3, target))
    println(randomSelectTake(3, target))
    println(randomSelectDrop(3, target))

    println(randomSelect(3, target))

    // e.g. List('e, 'd, 'a)
  }

  def randomSelectRec[A](n: Int, l: List[A]): List[A] = {
    def _recursion(cnt: Int, l: List[A], rst: List[A]): List[A] = Random.shuffle(l) match {
      case h :: tail if cnt < n => _recursion(cnt + 1, tail, rst :+ h)
      case _ if n == cnt => rst
    }

    _recursion(0, l, Nil)
  }

  def randomSelectTake[A](n: Int, l: List[A]): List[A] = Random.shuffle(l).take(n)

  def randomSelectDrop[A](n: Int, l: List[A]): List[A] = Random.shuffle(l).drop(l.length - n)

  def randomSelect[A](n: Int, l: List[A]): List[A] = {
    import P20.removeAt
    def recursion(cnt: Int, rst: List[A], l: List[A]): List[A] = l match {
      case _ if cnt < n => recursion(cnt + 1, rst :+ removeAt(Random.nextInt(l.length), l)._2, l.tail)
      case _ => rst
    }

    recursion(0, Nil, l)
  }

  // TODO Add case with p20 only do random at once
}
