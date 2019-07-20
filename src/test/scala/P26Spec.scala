import P26._
import org.scalatest.FunSpec

class P26Spec extends FunSpec {

  describe("combinations") {

    it("should generate sequence of sub-sequences that each length is n") {
      val (k, xs) = (3, List('a, 'b, 'c, 'd, 'e, 'f))

      val expected =
        List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), List('a, 'b, 'f), List('a, 'c, 'd), List('a, 'c, 'e), List('a, 'c, 'f), List('a, 'd, 'e), List('a, 'd, 'f), List('a, 'e, 'f),
          List('b, 'c, 'd), List('b, 'c, 'e), List('b, 'c, 'f), List('b, 'd, 'e), List('b, 'd, 'f), List('b, 'e, 'f),
          List('c, 'd, 'e), List('c, 'd, 'f), List('c, 'e, 'f),
          List('d, 'e, 'f))

      assert(combinations(k, xs) === expected)
    }

  }

}
