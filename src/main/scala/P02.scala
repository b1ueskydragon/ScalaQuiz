object P02 {
  def main(args: Array[String]) {
    lazy val target = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    lazy val target01 = List(1, 1, 2, 3, 5, 8)

    println(initPra(target))
    println(tailPra(target))

    println(penultimate(target01))

    println(penultimateRecursion(target))
    println(penultimateRecursion_(target))

    println(lastNth(2, target))
  }

  def initPra(l: List[Int]): List[Int] = {
    if (l.isEmpty) throw new NoSuchElementException
    l.init
  }

  def tailPra(l: List[Int]): List[Int] = {
    if (l.isEmpty) throw new NoSuchElementException
    l.tail
  }

  def penultimate[A](l: List[A]): A = {
    if (l.isEmpty) throw new NoSuchElementException
    l.init.last
  }

  def penultimateRecursion[A](l: List[A]): A = l match {
    case List(_, _) => l.head // if only 2 elements (base case).
    case list => penultimateRecursion(list.tail)

    case List(_) => throw new NoSuchElementException
  }

  // same algorithm as above.
  def penultimateRecursion_[A](l: List[A]): A = l match {
    case h :: List(t) => h
    case _ :: tail => penultimateRecursion_(tail) // OR case _ => penultimateRecursionBrief(l.tail) ???

    case _ => throw new NoSuchElementException
  }

  def lastNth[A](n: Int, l: List[A]): A = l match {
    case tail if tail.length == n => tail.head
    case _ :: tail => lastNth(n, tail)

    case _ => throw new NoSuchElementException
  }
}
