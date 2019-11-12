import P26._
import org.scalatest.FunSpec

class P26Spec extends FunSpec {

  describe("combinations") {

    it("should generate sequence of sub-sequences that each length is k") {
      val ns = List('a, 'b, 'c, 'd, 'e, 'f)
      val k = 3
      val expected = ns.combinations(k).toList
      assert(combinationsMutable(ns, k) === expected)
      assert(combinations(ns, k) === expected)
    }

  }

}
