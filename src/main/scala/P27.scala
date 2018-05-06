import P26.combinations

object P27 {
  def main(args: Array[String]): Unit = {
    val sample = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    println(group3(sample))
    // e.g. List[List[List[String]]] =
    // List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
  }

  // determine first (_,_) (_,_,_) then can get (_,_,_,_) automatically
  def group3[A](origin: List[A]): List[List[List[A]]] = {
    for {
      _2 <- combinations(2, origin)
      tail = origin diff _2
      _3 <- combinations(3, tail)
    } yield List(_2, _3, tail diff _3)
  }
}
