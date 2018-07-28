import P34._
import P37._

object P38 {
  def main(args: Array[String]): Unit = {
    lazy val given = 10090

    while (true) {
      timeWatch("P37")(phi(given)) // TODO Improvement be required
      timeWatch("P34")(totient(given))
    }
  }

  /**
    * : => Runtime evaluation of params (NOT a compile evaluation)
    *
    * Why doesn't use return type `Unit`?
    *
    * @param label label printed
    * @param block a function
    * @tparam A return type of function
    * @return
    */
  def timeWatch[A](label: String)(block: => A): A = {
    val start: Long = System.currentTimeMillis()
    val ret: A = block
    println(label + ": " + (System.currentTimeMillis() - start) + " ms.")
    ret
  }
}
