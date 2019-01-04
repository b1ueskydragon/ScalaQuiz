import scala.annotation.tailrec

object P04 {
  def main(args: Array[String]): Unit = {
    // val target = List()
    val target = "alexkingdom".toList
    println(length(target))
    println(length_(target))
    println(length__(target))
    println(length___(target))
    println(length____(target))
    println(length_____(target))
  }

  def length[A](l: List[A]): Int = l.length

  def length_[A](l: List[A]): Int = l match {
    case Nil => 0
    case _ :: tail => 1 + length_(tail)
  }

  def length__[A](l: List[A]): Int = l.zip(Stream.from(1, 0)).map(_._2).sum

  def length___[A](l: List[A]): Int = if (l.nonEmpty) l.zip(Stream.from(1)).last._2 else 0

  def length____[A](l: List[A]): Int = l.map(_ => 1).sum

  def length_____[A](l: List[A]): Int = {
    @tailrec
    def rec(l: List[A], acc: Int): Int = l match {
      case Nil => acc
      case _ :: tail => rec(tail, acc + 1)
    }

    rec(l, 0)
  }

}
