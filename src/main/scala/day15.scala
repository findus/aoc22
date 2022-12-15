import scala.util.control.Breaks.break

object day15 extends App {

  case class Sensor(position: Coord, manhattanDistance: Int)
  case class Coord(x:Int, y:Int)
  val regex = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r

  def calcManhattanDistance(a: Coord, b: Coord) = {
    (a.x - b.x).abs + (a.y - b.y).abs
  }

  def getLinesAlongSensor(sensor: Sensor) = {
    val wToc = ((sensor.position.x - sensor.manhattanDistance - 1) to sensor.position.x)
    val cTon = (sensor.position.y to (sensor.position.y - sensor.manhattanDistance - 1))
    val cToe = (sensor.position.x to (sensor.position.x + sensor.manhattanDistance + 1))
    val cTos = (sensor.position.y to (sensor.position.y + sensor.manhattanDistance + 1))
    val westToNorth = wToc.zip(cTon).map(entry => Coord(entry._1, entry._2)).toList
    val northToEast = cToe.zip(cTon.reverse).map(entry => Coord(entry._1, entry._2))
    val eastToSouth = cToe.reverse.zip(cTos).map(entry => Coord(entry._1, entry._2))
    val southToWest = wToc.reverse.zip(cTos.reverse).map(entry => Coord(entry._1, entry._2))
    val coords = List(westToNorth, northToEast, eastToSouth, southToWest).flatten
    coords
  }

  def isOccupied(sensors: List[Sensor], line: Int, range: Range, beacons: List[Coord], predicate: (Sensor, Coord) => Boolean) = {
    val lineCoords = range.map(idx => Coord(idx, line)).toList
    val distanceBetweenSensorsAndLine = lineCoords
      .filter(coord => !beacons.contains(coord))
      .map(l => {
        sensors.map(s => (l, predicate(s,l)))
      })
    distanceBetweenSensorsAndLine
  }

  def isOccupied(sensors: List[Sensor], coord: Coord, predicate: (Sensor, Coord) => Boolean): List[(Coord, Boolean)] = {
    val ee = sensors.map(s => (coord, predicate(s,coord))).toList
    ee
  }


  io.load("day15") { lines =>
    val parsed = lines.map {
      case regex(sx,sy,bx,by) => (Coord(Integer.parseInt(sx),Integer.parseInt(sy)),Coord(Integer.parseInt(bx),Integer.parseInt(by)))
      case _ => throw new Exception("Parsing failed")
    }

    val positions = parsed
      .map(position => Sensor(Coord(position._1.x, position._1.y), calcManhattanDistance(position._1, position._2)))

    val predicate: (Sensor, Coord) => Boolean = (s,l) => calcManhattanDistance(s.position, l) <= s.manhattanDistance

    val lowestX = positions.map(pos => pos.position.x - pos.manhattanDistance).min
    val highestX = positions.map(pos => pos.position.x + pos.manhattanDistance).max
    val range = (lowestX to highestX)
    val beacons = parsed.map(_._2)
    val occupied = isOccupied(positions, 2000000, range, beacons, (s,l) => calcManhattanDistance(s.position, l) <= s.manhattanDistance)
      .filter(list => list.exists(_._2)).map(_.head._1)
    println(occupied.size)

    val box = 4000000

    val spacebeneathSensors = positions.flatMap(getLinesAlongSensor).distinct.filter(coord => coord.x >= 0 && coord.x <= box && coord.y >= 0 && coord.y <= box)

    for (coord <- spacebeneathSensors) {
      val efffe = isOccupied(positions, coord, predicate)
      if (efffe.map(_._2).count(e => e == true) == 0) {
        println((efffe.head._1.x.toLong * 4000000L) + efffe.head._1.y.toLong)
      }
    }


    }

}