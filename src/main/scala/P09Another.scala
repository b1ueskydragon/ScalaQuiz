
/**
  * P09 の重複を許さないパターン
  */
object P09Another {
  def main(args: Array[String]) = {
    val duplicatedList = List('a, 'a, 'a, 'b, 'b, 'c, 'a, 'c)
    println(pack(duplicatedList))
  }

  /**
    *
    * @param unpacked a target list unpacked yet (elements of outer)
    * @tparam A any type
    * @return outer
    */
  def pack[A](unpacked: List[A]): List[List[A]] = {

    var outer: List[List[A]] = List() // ここに格納
    var table: Map[A, Int] = Map() // 要素と outer での位置をマッピング

    for (i <- unpacked) {
      if (outer.isEmpty) {
        table += (i -> 0)
        outer = outer :+ List(i)
      }
      else if (!outer.contains(List(i))) {
        outer = outer :+ List(i)
        val lastIdx = outer.size - 1
        table += (i -> lastIdx)
      }
      else {
        val toPutIdx = table.getOrElse(i, 0)
        println(i, toPutIdx)
      }
    }

    outer
  }
}
