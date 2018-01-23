object P02 {
  def main(args: Array[String]) {
    println(initPra(Dummy.yourList))
    println(tailPra(Dummy.yourList))

    println(penultimate(Dummy.myList))

    println(penultimateRecursion(Dummy.yourList))
    println(penultimateRecursion_(Dummy.yourList))

    println(lastNth(2, Dummy.yourList))
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
    case List(a, b) => l.head // if only 2 elements (base case).
    case list => penultimateRecursion(list.tail)

    case List(a) => throw new NoSuchElementException
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
