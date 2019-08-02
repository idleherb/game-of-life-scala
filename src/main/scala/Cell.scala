case class Cell(val alive: Boolean) {
  def step(rule: Rule, neighbours: Int): Cell = {
    if (alive) handleAlive(neighbours, rule) else handleDead(neighbours, rule)
  }

  private def handleAlive(neighbours: Int, rule: Rule): Cell = {
    Cell(alive = rule.survive contains neighbours)
  }

  private def handleDead(neighbours: Int, rule: Rule): Cell = {
    Cell(alive = rule.birth contains neighbours)
  }
}
