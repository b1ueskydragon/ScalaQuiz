import P26.combinations

object P27 {
  def main(args: Array[String]): Unit = {
    val sample = List("A", "B", "C", "D", "E", "F", "G", "H", "I")
    //println(group3(sample))

    val pattern = List(2, 3, 4)
    //println(group3n(pattern, sample))

    println(groupN(pattern, sample))
  }

  // determine first (_,_) (_,_,_) then can get (_,_,_,_) automatically
  def group3[A](origin: List[A]): List[List[List[A]]] = {
    for {
      _2 <- combinations(2, origin)
      tail = origin diff _2
      _3 <- combinations(3, tail)
    } yield List(_2, _3, tail diff _3)
  }

  def groupN[A](pattern: List[Int], origin: List[A]): List[List[List[A]]] = pattern match {
    case Nil => List(List())
    case h :: tail =>
      combinations(h, origin) flatMap {
        c =>
          groupN(tail, origin diff c) map {
            c :: _
          }
      }
  }
}
