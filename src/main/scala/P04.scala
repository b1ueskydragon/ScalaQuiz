
object P04 {
  def main(args: Array[String]): Unit = {
    val target = List(1, 1, 2, 3, 5, 8)
    println(length(target))
    println(length_(target))
  }

  def length[A](l: List[A]): Int = l.length

  def length_[A](l: List[A]): Int = l match {
    case Nil => 0
    case _ :: tail => 1 + length_(tail)
  }
}
