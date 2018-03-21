/**
  * Pack consecutive duplicates of list elements into sublists.
  *
  * 重複を除いている訳ではない
  */
object P09 {
  def main(args: Array[String]) = {
    lazy val duplicatedList = List('a, 'a, 'a, 'b, 'b, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(pack(List(), duplicatedList))
  }

  // initial approach.
  def __pack[A](l: List[A]): List[List[A]] = l match {
    case Nil => Nil
    case List(a) => List(List(a)) // base case
    case List(a, b) => List(List(a), List(b))
  }

  /**
    * 重複を許すパターン (破壊的?)
    *
    * @param outer    bigger one (initial status is Nil)
    * @param unpacked a target list unpacked yet (elements of outer)
    * @tparam A any type
    * @return outer
    */
  def pack[A](outer: List[List[A]], unpacked: List[A]): List[List[A]] = unpacked match {
    /*
     * exit case
     *
     * 着目が全て終わったら(h::tail の これ以上返せる tail がなかったら)結果を返す
     */
    case Nil => outer

    /*
     * standard if case
     *
     * outer がまだ empty の時を含め、
     * h に対する pack がまだない -> 新しい pack を append する
     */
    case h :: tail
      if outer.isEmpty || outer.last.head != h =>
      pack(outer ::: List(List(h)), tail)

    /*
     * standard else case
     *
     * h に対する pack がすでに(末尾に)存在する -> 該当 pack に入れる
     * (init: last を除いたこれまでの要素)
     */
    case h :: tail =>
      pack(outer.init ::: List(outer.last ::: List(h)), tail)
  }
}
