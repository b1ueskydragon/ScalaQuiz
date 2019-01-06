import scala.annotation.tailrec

object P02 {
  def main(args: Array[String]) {
    lazy val target = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    println(penultimate(target))
    println(penultimate_(target))
    println(penultimate__(target))
    println(penultimate___(target))
  }

  def penultimate[A](l: List[A]): A = {
    if (l.isEmpty) throw new NoSuchElementException
    l.init.last
  }

  @tailrec
  def penultimate_[A](l: List[A]): A = l match {
    case List(_, _) => l.head // if only 2 elements (base case).
    case list => penultimate_(list.tail)
    case List(_) => throw new NoSuchElementException
  }

  // same algorithm as above.
  @tailrec
  def penultimate__[A](l: List[A]): A = l match {
    case h :: List(_) => h
    case _ :: tail => penultimate__(tail)
    case _ => throw new NoSuchElementException
  }

  @tailrec
  def penultimate___[A](l: List[A]): A = l match {
    case _ if l.length == 2 => l.head
    case xs => penultimate___(xs.tail)
    case _ => throw new NoSuchElementException
  }

}
