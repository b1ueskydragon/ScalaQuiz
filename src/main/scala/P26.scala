import scala.collection.mutable

/** Generate all the possibilities C(N,K)
  *
  * N := len(xs) | xs is a given list
  * k := len(ps) | ps is a sub-sequence of xs
  */
object P26 {

  def combinations[A](n: Int, xs: List[A]): List[List[A]] = {
    val res = new mutable.MutableList[List[A]]

    def dfsGen(curr: List[A], ps: List[A]) {
      val k = ps.length

      if (curr.isEmpty && k < n) return
      curr match {
        case _ if k == n => res += ps.reverse
        case h :: tail =>
          dfsGen(tail, h :: ps)
          dfsGen(tail, ps)
      }
    }

    dfsGen(xs, List())
    res.toList
  }

}
