import P34._
import P37._

object P38 {
  def main(args: Array[String]): Unit = {
    lazy val given = 10090
    lazy val res34 = totient(given)
    lazy val res37 = phi(given)

    val start34 = System.currentTimeMillis()
    res34
    val end34 = System.currentTimeMillis()
    println(end34 - start34)

    val start37 = System.currentTimeMillis()
    res37
    val end37 = System.currentTimeMillis()
    println(end37 - start37)

    // TODO Make clock (OR time) function
  }
}
