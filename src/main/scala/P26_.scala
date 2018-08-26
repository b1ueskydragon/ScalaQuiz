object P26_ {

  def flatMapSublists[I, O](ls: List[I])(f: List[I] => List[O]): List[O] = ls match {
    case Nil => Nil
    case _ :: tail => f(ls) ::: flatMapSublists(tail)(f)
  }

  def main(args: Array[String]): Unit = {
    // test
    val testList = List('a, 'b, 'c, 'd, 'e)
    val res00 = flatMapSublists(testList)(sl => sl)
    println(res00)
  }
}
