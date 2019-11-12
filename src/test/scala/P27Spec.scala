import P27.{group3, groupN}
import org.scalatest.FunSpec

class P27Spec extends FunSpec {

  describe("groupN and pattern is 2, 3, 4") {
    it("should return 3 disjoint subgroups of 2, 3 and 4 length") {
      val ls = List("A", "B", "C", "D", "E", "F", "G", "H", "I")
      val pattern = List(2, 3, 4)
      assert(groupN(pattern, ls) === group3(ls))
    }
  }

}
