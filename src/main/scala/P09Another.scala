import scala.collection.mutable

/**
  * P09 の重複を許さないパターン
  */
object P09Another {
  def main(args: Array[String]) = {
    val duplicatedList = List('a, 'a, 'a, 'b, 'b, 'a)
    println(pack(List(), duplicatedList, new mutable.HashMap))
  }

  /**
    *
    * @param outer    bigger one (initial status is Nil)
    * @param unpacked a target list unpacked yet (elements of outer)
    * @param table    着目時点での h とその格納場所(インデックス)
    * @tparam A any type
    * @return outer
    */
  def pack[A](outer: List[List[A]], unpacked: List[A], table: mutable.HashMap[A, Int]): List[List[A]] = unpacked match {
    case Nil => outer

    case h :: tail if outer.isEmpty =>
      table += h -> 1
      println(table)
      pack(outer ::: List(List(h)), tail, table)

    case h :: tail  if !table.contains(h) =>
        table += h -> (outer.size - 1)
        println(table)
        pack(outer ::: List(List(h)), tail, table)

    case h :: tail =>

      pack(outer ::: List(List(h)), tail, table)
  }

}
