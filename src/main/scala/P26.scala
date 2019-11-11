/** Generate all the possibilities C(N,K)
  *
  * N := len(xs) | xs is a given list
  * k := len(ps) | ps is a sub-sequence of xs
  */
object P26 {

  def combinationsMutable[A](n: Int, xs: List[A]): List[List[A]] = {
    val res = scala.collection.mutable.ListBuffer.empty[List[A]]

    def dfs(curr: List[A], ps: List[A]) {
      val k = ps.length
      if (curr.isEmpty && k < n) return
      curr match {
        case _ if k == n => res.append(ps)
        case h :: tail =>
          dfs(tail, ps ::: List(h))
          dfs(tail, ps)
      }
    }

    dfs(xs, List())
    res.toList
  }

  def combinations[A](n: Int, xs: List[A]): List[List[A]] = {
    /** T => S, with concat S to itself.
      *
      * note: type of f(ts) is List[S]
      *
      * @param ts list of type T
      * @param f  mapping. convert all elements of T to S
      * @return list of type S
      */
    def flatMapLike[T, S](ts: List[T])(f: List[T] => List[S]): List[S] =
      if (ts.isEmpty) List.empty[S]
      else f(ts) ::: flatMapLike(ts.tail)(f)

    if (n == 0) List(Nil)
    else ???
  }

}
