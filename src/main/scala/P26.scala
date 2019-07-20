/** Generate all the possibilities C(N,K)
  *
  * N := len(xs) | xs is a given list
  * k := len(ps) | ps is a sub-sequence of xs
  */
object P26 {

  def combinations[A](n: Int, xs: List[A]): List[List[A]] = {
    val res = scala.collection.mutable.ListBuffer.empty[List[A]]

    def dfsGen(curr: List[A], ps: List[A]) {
      val k = ps.length
      if (curr.isEmpty && k < n) return
      curr match {
        case _ if k == n => res.append(ps)
        case h :: tail  =>
          dfsGen(tail, ps ::: List(h))
          dfsGen(tail, ps)
      }
    }

    dfsGen(xs, List())
    res.toList
  }

}
