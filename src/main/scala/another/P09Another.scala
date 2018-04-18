package another


/**
  * P09 の重複を許さないパターン
  *
  * for 文と違い collection によるループはリストを破壊せず(新しく作るので)実装できる.
  * 副作用防止にも繋がる.
  *
  */
object P09Another {
  def main(args: Array[String]) = {
    val duplicatedList = List('a, 'a, 'a, 'b, 'b, 'c, 'a, 'c)
    println(pack(duplicatedList))
  }

  /**
    * outer においてとても破壊的
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
      else if (!table.contains(i)) {
        outer = outer :+ List(i)
        val lastIdx = outer.size - 1
        table += (i -> lastIdx)
      }
      else {
        val toPutIdx: Int = table.getOrElse(i, 0)

        outer.patch(toPutIdx, Seq(outer(toPutIdx) ::: List(i)), 1) // immutable...
        // TODO Immutable List + collection で実装する.
      }
    }

    outer
  }
}
