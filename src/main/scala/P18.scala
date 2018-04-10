object P18 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(slice(3, 7, target)) //List('d, 'e, 'f, 'g)
  }

  def slice[A](to: Int, from: Int, l: List[A]): List[Any] = l.map {
    case e if l.indexOf(e) >= to && l.indexOf(e) < from => e
    case _ => ""
  }.filter(e => e != "")
}
