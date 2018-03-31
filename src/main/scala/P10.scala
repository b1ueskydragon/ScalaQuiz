object P10 {
  def main(args: Array[String]): Unit = {
    val duplicatedList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encode(duplicatedList))
  }

  def pack[A](l: List[A]): List[List[A]] = {
    def _pack(outer: List[List[A]], unpacked: List[A]): List[List[A]] = unpacked match {
      case Nil => outer
      case h :: tail // just joining
        if outer.isEmpty || outer.last.head != h =>
        _pack(outer ::: List(List(h)), tail)
      case h :: tail // put into the last pack since same as previous one
      => _pack(outer.init ::: List(outer.last ::: List(h)), tail)
    }

    _pack(List(), l)
  }

  def encode[A](packed: List[A]): List[(Int, A)] = {
    def _encode(tupList: List[(Int, A)], packed: List[List[A]]): List[(Int, A)] = packed match {
      case Nil => tupList
      case h :: tail => _encode(tupList ::: List((h.length, h.head)), tail)
    }

    _encode(List(), pack(packed))
  }
}
