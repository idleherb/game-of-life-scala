import org.scalatest.{FeatureSpec, Matchers}

class TestGrid extends FeatureSpec with Matchers {

  private def makeGrid(v: Vector[Boolean]) = {
    Grid(
      width = 3,
      height = 3,
      v = v.map(Cell),
    )
  }

  private val emptyGrid = makeGrid(Vector(
    false, false, false,
    false, false, false,
    false, false, false,
  ))

  private val defaultRule = Rule.parse("B3/S23").get

  feature("Grid") {
    scenario("Given the default rule, a grid with only dead cells doesn't change in a step") {
      emptyGrid.step(defaultRule) shouldBe emptyGrid
    }

    scenario("An alive cell with no neighbours dies. Outside grid cells are dead by default.\"") {
      val grid1 = makeGrid(Vector(
        false, false, false,
        false, true,  false,
        false, false, false,
      ))
      grid1.step(defaultRule) shouldBe emptyGrid

      val grid2 = makeGrid(Vector(
        true,  false, true,
        false, false, false,
        true,  false, true,
      ))
      grid2.step(defaultRule) shouldBe emptyGrid
    }

    scenario("An alive cell with two or three neighbours lives on") {
      val grid1 = makeGrid(Vector(
        true,  true,  false,
        true,  false, false,
        false, false, false,
      ))
      grid1.step(defaultRule) shouldBe makeGrid(Vector(
        true,  true,  false,
        true,  true, false,
        false, false, false,
      ))

      val grid2 = makeGrid(Vector(
        true,  false, true,
        false, true,  false,
        true,  false, false,
      ))
      grid2.step(defaultRule) shouldBe makeGrid(Vector(
        false, true,  false,
        true,  true,  false,
        false, false, false,
      ))
    }

    scenario("An alive cell with more than three neighbours dies") {
      val grid = makeGrid(Vector(
        true,  false, true,
        false, true,  false,
        true,  false, true,
      ))
      grid.step(defaultRule) shouldBe makeGrid(Vector(
        false, true,  false,
        true,  false, true,
        false, true,  false,
      ))
    }

    scenario("A dead cell with exactly three neighbours comes to life") {
      val grid = makeGrid(Vector(
        true,  false, true,
        false, false, false,
        true,  false, false,
      ))
      grid.step(defaultRule) shouldBe makeGrid(Vector(
        false, false, false,
        false, true,  false,
        false, false, false,
      ))
    }

  }
}
