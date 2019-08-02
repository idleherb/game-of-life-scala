import org.scalatest.{FeatureSpec, Matchers}

class TestRule extends FeatureSpec with Matchers {
  feature("Rule") {
    scenario("Ill-formatted rules parse to None") {
      Rule.parse("B23/S6C") shouldBe empty
      Rule.parse("S23/B6") shouldBe empty
      Rule.parse("@R1$!") shouldBe empty
      Rule.parse("foobar") shouldBe empty
    }

    scenario("RLE-formatted rules can be parsed") {
      Rule.parse("B1357/S1357").foreach(rule => {
        rule.birth shouldBe Seq(1, 3, 5, 7)
        rule.survive shouldBe Seq(1, 3, 5, 7)
      })
      Rule.parse("B2/S").foreach(rule => {
        rule.birth shouldBe Seq(2)
        rule.survive shouldBe Seq()
      })
      Rule.parse("B3/S012345678").foreach(rule => {
        rule.birth shouldBe Seq(3)
        rule.survive shouldBe Seq(0, 1, 2, 3, 4, 5, 6, 7, 8)
      })
    }
  }
}
