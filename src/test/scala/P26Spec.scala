import P26._
import org.scalatest.FunSpec

class P26Spec extends FunSpec {

  describe("combinations") {

    it("should generate sequence of sub-sequences that each length is k") {
      val (k, xs) = (3, List('a, 'b, 'c, 'd, 'e, 'f))
      assert(combinations(k, xs) === xs.combinations(k).toList)
    }

  }

}
