object P02 {
  def main(args: Array[String]) {
    println(penultimate(Dummy.myList))
    print(initPra(Dummy.myList))
  }

  def penultimate[A](l: List[A]): A = {
    if (l.isEmpty) throw new NoSuchElementException
    l.init.last
  }

  def initPra(l: List[Int])= {
    if (l.isEmpty) throw new NoSuchElementException
    l.init
  }
}
