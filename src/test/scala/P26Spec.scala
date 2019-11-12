import P26._
import org.scalatest.FunSpec

class P26Spec extends FunSpec {

  describe("combinations") {

    it("should generate sequence of sub-sequences that each length is k") {
      val (ns, k) = (List('a, 'b, 'c, 'd, 'e, 'f), 3)
      assert(combinationsMutable(ns, k) === ns.combinations(k).toList)
    }

  }

}
