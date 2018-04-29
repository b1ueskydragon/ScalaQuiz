import P23.randomSelectOnce

object P25 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f)
    println(randomPermuteSimple(target))

    // e.g. List('b, 'a, 'd, 'c, 'e, 'f)
  }

  // simply implementation. O(n^2)
  def randomPermuteSimple[A](origin: List[A]): List[A] = randomSelectOnce(origin.length, origin)
}
