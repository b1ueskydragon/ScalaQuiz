object P27 {
  // determine first (_,_) (_,_,_) then can get (_,_,_,_) automatically.
  // (A, B) is the same solution as (B, A), so use combinations.
  def group3[A](ns: List[A]): List[List[List[A]]] = {
    for {
      x <- ns.combinations(2).toList
      notX = ns diff x
      y <- notX.combinations(3)
      notXY = notX diff y
    } yield List(x, y, notXY)
  }

  def groupN[A](pattern: List[Int], ns: List[A]): List[List[List[A]]] = pattern match {
    case Nil => List(Nil)
    case k :: ks => ns.combinations(k).toList.flatMap(xs => groupN(ks, ns diff xs) map (xs :: _))
  }
}
