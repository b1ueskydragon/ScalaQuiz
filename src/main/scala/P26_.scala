object P26_ {

  def cdrs[A](ls: List[A])(f: List[A] => List[List[A]]): List[List[A]] = ls match {
    case Nil => Nil
    case _ :: tail => f(ls) ::: cdrs(tail)(f)
  }

  def combinations[A](n: Int, ori: List[A]): List[List[A]] = {
    if (n == 0) List(Nil)
    else cdrs(ori) { sl =>
      combinations(n - 1, sl.tail) map { c =>
        sl.head :: c // append a head to an element(List[A]) from combinations.
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val datum = List('a, 'b, 'c, 'd, 'e)
    // test
     val res00 = cdrs(datum)(sl => List(sl))
     println(res00)

    val res01 = combinations(3, datum)
    println(res01)
  }
}
