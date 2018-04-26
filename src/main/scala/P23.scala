import scala.util.Random

import P20.removeAt

object P23 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)
    println(randomSelectRec(3, target))
    println(randomSelectTake(3, target))
    println(randomSelectDrop(3, target))

    println(randomSelectEach(3, target))
    println(randomSelectOnce(3, target))

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

  def randomSelectEach[A](n: Int, l: List[A]): List[A] = {
    def recursion(cnt: Int, rst: List[A], l: List[A]): List[A] = l match {
      case _ if cnt < n => recursion(cnt + 1, rst :+ removeAt(Random.nextInt(l.length), l)._2, l.tail)
      case _ => rst
    }

    recursion(0, Nil, l)
  }

  // case with p20 only do random at once
  def randomSelectOnce[A](n: Int, l: List[A]): List[A] = {
    def _recursion(n: Int, l: List[A], res: List[A]): List[A] = {
      if (n > 0) {
        val (rest, el) = removeAt(Random.nextInt(l.length), l)
        _recursion(n - 1, rest, el :: res)
      } else res
    }

    _recursion(n, l, Nil)
  }
}
