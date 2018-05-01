object P26 {
  def main(args: Array[String]): Unit = {
    val sample = List('a, 'b, 'c, 'd, 'e, 'f)
    println(combinations(3, sample))

    // generate all the possibilities C(N,K)
    // List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  }

  def combinations[A](n: Int, origin: List[A]): List[List[A]] = {

  }
}
