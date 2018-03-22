
/**
  * P09 の重複を許さないパターン
  */
object P09Another {
  def main(args: Array[String]) = {
    val duplicatedList = List('a, 'a, 'a, 'b, 'b, 'a, 'c)
    println(pack(duplicatedList))
  }

  /**
    *
    * @param unpacked a target list unpacked yet (elements of outer)
    * @tparam A any type
    * @return outer
    */
  def pack[A](unpacked: List[A]): List[List[A]] = {

    var outer: List[List[A]] = List()
    var table: Map[A, Int] = Map()

    for (i <- unpacked) {

      if (!outer.contains(List(i))) {
        outer = outer :+ List(i)
        table += (i -> 1)

      } else {
        val newCnt = table.getOrElse(i, 1) + 1
        table += (i -> newCnt)
      }

    }
    outer
  }
}
