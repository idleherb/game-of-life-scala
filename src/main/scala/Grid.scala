case class Coords(val x: Int, val y: Int)

case class Grid(val width: Int, val height: Int, val v: Vector[Cell]) {
  def step(rule: Rule): Grid = {
    val stepV: Vector[Cell] = (v
      .zipWithIndex
      .map {
        case (cell, i) => cell.step(rule, countAliveNeighbours(cellCoords(i)))
      })
    Grid(width, height, stepV)
  }

  private def countAliveNeighbours(coords: Coords): Int = {
    val Coords(x, y) = coords;
    val neighbours = Vector(
      cellAt(Coords(x - 1, y - 1)), cellAt(Coords(x, y - 1)), cellAt(Coords(x + 1, y - 1)),
      cellAt(Coords(x - 1, y)),                               cellAt(Coords(x + 1, y)),
      cellAt(Coords(x - 1, y + 1)), cellAt(Coords(x, y + 1)), cellAt(Coords(x + 1, y + 1)),
    )
    neighbours
      .flatMap(identity)
      .count(_.alive)
  }

  private def cellCoords(i: Int): Coords = {
    Coords(i % width, i / width)
  }

  private def cellIndex(coords: Coords): Option[Int] = {
    if ((0 <= coords.x) && (coords.x < width) &&
        (0 <= coords.y) && (coords.y < height))  {
      Some(coords.y * width + coords.x)
    } else {
      None
    }
  }

  def cellAt(coords: Coords): Option[Cell] = {
    cellIndex(coords).map(v)
  }
}

object Grid {
  def withRandomCells(width: Int, height: Int) = {
    val v = (0 until width * height)
      .map(_ => randCell())
      .toVector
    Grid(width, height, v);
  }

  private def randCell() = {
    Cell((Math.random() < 0.5))
  }
}
