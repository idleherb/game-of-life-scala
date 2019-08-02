object Main extends App {

  private def renderGrid(grid: Grid) = {
    (0 until grid.height).map(y => {
      (0 until grid.width).map(x => {
        val cell = grid.cellAt(Coords(x, y)).get
        if (cell.alive) "▓" else "."
      }).mkString("")
    }).mkString("\n")
  }

  private def printGrid(grid: Grid, rule: Rule, times: Int, generation: Int = 1): Unit = {
    if (times == 0) {
      ()
    } else {
      println(s"\n\n===== Generation: $generation =====\n\n")
      println(renderGrid(grid))
      Thread.sleep(50)
      printGrid(grid.step(rule), rule, times - 1, generation + 1)
    }
  }

  val rule = Rule.parse(args(2))
  rule.foreach(r => {
    val grid = Grid.withRandomCells(args(0).toInt, args(1).toInt)
    printGrid(grid, r, args(3).toInt)
  })
}
