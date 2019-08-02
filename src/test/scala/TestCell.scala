import org.scalatest.{FeatureSpec, Matchers}

class TestCell extends FeatureSpec with Matchers {
  feature("cell") {
    scenario("run one step given the default rule") {
      Rule.parse("B3/S23").foreach(rule => {
        Cell(false).step(rule, 0).alive shouldBe false
        Cell(false).step(rule, 1).alive shouldBe false
        Cell(false).step(rule, 2).alive shouldBe false
        Cell(false).step(rule, 3).alive shouldBe true
        Cell(false).step(rule, 4).alive shouldBe false
        Cell(false).step(rule, 5).alive shouldBe false
        Cell(false).step(rule, 6).alive shouldBe false
        Cell(false).step(rule, 7).alive shouldBe false
        Cell(false).step(rule, 8).alive shouldBe false

        Cell(true).step(rule, 0).alive shouldBe false
        Cell(true).step(rule, 1).alive shouldBe false
        Cell(true).step(rule, 2).alive shouldBe true
        Cell(true).step(rule, 3).alive shouldBe true
        Cell(true).step(rule, 4).alive shouldBe false
        Cell(true).step(rule, 5).alive shouldBe false
        Cell(true).step(rule, 6).alive shouldBe false
        Cell(true).step(rule, 7).alive shouldBe false
        Cell(true).step(rule, 8).alive shouldBe false
      })
    }
  }
}
