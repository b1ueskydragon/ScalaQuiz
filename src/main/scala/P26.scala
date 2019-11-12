/** Generate all the possibilities C(N,K)
  *
  * N := len(xs) | xs is a given list
  * k := len(ps) | ps is a sub-sequence of xs
  */
object P26 {

  def combinationsMutable[A](ns: List[A], k: Int): List[List[A]] = {
    val res = scala.collection.mutable.ListBuffer.empty[List[A]]

    def dfs(rem: List[A], leaf: List[A]) {
      val size = leaf.length
      if (rem.isEmpty && size < k) return
      rem match {
        case _ if size == k => res.append(leaf)
        case h :: tail =>
          dfs(tail, leaf ::: List(h))
          dfs(tail, leaf)
      }
    }

    dfs(ns, List())
    res.toList
  }

  /**
    * n C k
    *
    * @param ns
    * @param k
    * @tparam A
    * @return
    */
  def combinations[A](ns: List[A], k: Int): List[List[A]] = {
    /** T => S, with concat S to itself.
      *
      * note: type of f(ts) is List[S]
      *
      * @param ts list of type T
      * @param f  mapping. convert all elements of T to S
      * @return list of type S
      */
    def flatMapLike[T, S](ts: List[T])(f: List[T] => List[S]): List[S] = {
      if (ts.isEmpty) Nil
      else f(ts) ::: flatMapLike(ts.tail)(f)
    }

    if (k == 0) List(Nil)
    else flatMapLike(ns) { xs =>
      // Backtracking.
      // cs is an each elements of successful result (previous combinations)
      // so next step is just concat a head to previous results.
      combinations(xs.tail, k - 1).map(cs => xs.head :: cs)
    }
  }

}
