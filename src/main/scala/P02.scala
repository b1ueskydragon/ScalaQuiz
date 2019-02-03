import scala.annotation.tailrec

class P02[A](val xs: List[A]) {
  def isValid: Boolean = xs.length > 1
}

object P02 {

  implicit def p02[A](xs: List[A]): P02[A] = new P02[A](xs)

  def main(args: Array[String]) {
    val xs = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    if (xs.isValid) {
      println(penultimate(xs))
      println(penultimate_(xs))
      println(penultimate__(xs))
      println(penultimate___(xs))
    }
  }

  def penultimate[A](xs: List[A]): A = xs.init.last

  @tailrec
  def penultimate_[A](l: List[A]): A = l match {
    case List(_, _) => l.head // if only 2 elements (base case).
    case xs => penultimate_(xs.tail)
  }

  @tailrec
  def penultimate__[A](l: List[A]): A = l match {
    case h :: List(_) => h
    case _ :: tail => penultimate__(tail)
  }

  @tailrec
  def penultimate___[A](l: List[A]): A = l match {
    case _ if l.length == 2 => l.head
    case xs => penultimate___(xs.tail)
  }

}
