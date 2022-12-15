object day15 extends App {

  case class Sensor(position: Coord, manhattanDistance: Int)
  case class Coord(x:Int, y:Int)
  val regex = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r

  def calcManhattanDistance(a: Coord, b: Coord) = {
    (a.x - b.x).abs + (a.y - b.y).abs
  }

  def isOccupied(sensors: List[Sensor], line: Int, range: Range, beacons: List[Coord]) = {
    println(range)
    val lineCoords = range.map(idx => Coord(idx, line)).toList
    val distanceBetweenSensorsAndLine = lineCoords
      .filter(coord => !beacons.contains(coord))
      .map(l => {
        println(l)
        sensors.map(s => calcManhattanDistance(s.position, l) <= s.manhattanDistance)
      })
      .filter(list => list.contains(true))
    distanceBetweenSensorsAndLine
  }

  io.load("day15") { lines =>
    val parsed = lines.map {
      case regex(sx,sy,bx,by) => (Coord(Integer.parseInt(sx),Integer.parseInt(sy)),Coord(Integer.parseInt(bx),Integer.parseInt(by)))
      case _ => throw new Exception("Parsing failed")
    }

    val positions = parsed
      .map(position => Sensor(Coord(position._1.x, position._1.y), calcManhattanDistance(position._1, position._2)))


    val lowestX = positions.map(pos => pos.position.x - pos.manhattanDistance).min
    val highestX = positions.map(pos => pos.position.x + pos.manhattanDistance).max
    val range = (lowestX to highestX)
    val beacons = parsed.map(_._2)
    val d = isOccupied(positions, 2000000, range, beacons)
    println(d.size)
  }

  io.load("day15") { lines =>

  }
}