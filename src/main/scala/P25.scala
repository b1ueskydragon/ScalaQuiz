import P23.randomSelectOnce

import scala.reflect.ClassTag

object P25 {
  def main(args: Array[String]): Unit = {
    val target = List('a, 'b, 'c, 'd, 'e, 'f)
    println(randomPermuteSimple(target))
    println(randomPermute(target))

    // e.g. List('b, 'a, 'd, 'c, 'e, 'f)
  }

  // simply implementation. O(n^2)
  def randomPermuteSimple[A](origin: List[A]): List[A] = randomSelectOnce(origin.length, origin)

  // O(n)
  def randomPermute[A: ClassTag](origin: List[A]): List[A] = {
    val rnd = new util.Random
    val ary = origin.toArray // make mutable

    for (i <- ary.length - 1 to 1 by -1) { // reverse
      val rndIdx = rnd.nextInt(i + 1)
      val currVal = ary(i) // tmp

      // swap
      ary.update(i, ary(rndIdx))
      ary.update(rndIdx, currVal)
    }

    ary.toList
  }
}
