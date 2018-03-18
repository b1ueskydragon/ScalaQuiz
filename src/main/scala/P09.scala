/**
  * Pack consecutive duplicates of list elements into sublists.
  */
object P09 {
  def main(args: Array[String]) = {
    lazy val duplicatedList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    println(pack(List('a)))
    println(_pack(List(), duplicatedList))
  }

  def pack[A](l: List[A]): List[List[A]] = l match {
    case Nil => Nil
    case List(a) => List(List(a)) // base case
    case List(a, b) => List(List(a), List(b))
  }

  /**
    *
    * @param res a result (List of lists)
    * @param rem an element of List of lists
    * @tparam A  any type
    * @return    res (recursion)
    */
  def _pack[A](res: List[List[A]], rem: List[A]): List[List[A]] = rem match {
    case Nil => res
    case h::tail
      if res.isEmpty || res.last.head != h => _pack(res:::List(List(h)), tail)
    case h::tail => _pack(res.init:::List(res.last:::List(h)), tail)
  }
}
