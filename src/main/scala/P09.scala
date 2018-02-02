/**
  * Pack consecutive duplicates of list elements into sublists.
  */
object P09 {
  def main(args: Array[String]): Unit = {
    println(pack(Dummy.duplicatedList2))
  }

  def pack[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    //case
  }
}
