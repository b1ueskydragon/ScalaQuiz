import scala.annotation.tailrec

/**
  * Eliminate consecutive duplicates of list elements.
  * The order of the elements should not be changed.
  */
object P08 {
  def main(args: Array[String]): Unit = {
    // val target = List('a', 'a', 'b', 'b', 'a', 'a', 'c', 'a', 'c', 'c', 'a', 'd', 'a')
    val target = "aaabccaadeeee".toList
    println(compress(target))
    println(compress__(target))
    println(compress___(target))
    println(compress____(target))
  }

  def compress[A](l: List[A]): List[A] = {
    @tailrec
    def rec(l: List[A], stack: List[A]): List[A] = l match {
      case Nil => Nil
      case List(x) => x :: stack
      case h :: tail => if (h == tail.head) rec(tail, stack) else rec(tail, h :: stack)
    }

    rec(l, List()).reverse
  }

  def compress__[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: List() => List(h) // exit case. 一つしかない場合そのまま返す
    case h :: tail if h == tail.head => compress__(tail) // standard case 1. head と tail.head が同じなら tail.head を持ってきて 元の head をとる
    case h :: tail => h :: compress__(tail) // standard case 2. head と tail.head が異なる場合, 何もしない (そのままくっつける)
  }

  def compress___[A](l: List[A]): List[A] = {
    @tailrec
    def rec(acc: List[A], l: List[A]): List[A] = l match {
      case Nil => acc.reverse
      case h :: tail => rec(h :: acc, tail.dropWhile(h == _))
    }

    rec(List(), l)
  }

  def compress____[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: tail => h :: compress____(tail.dropWhile(h == _))
  }

  def compress_____[A](l: List[A]): List[A] = ??? // TODO foldr

}
