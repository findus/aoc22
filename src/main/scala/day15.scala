import scala.util.control.Breaks.break

object day15 extends App {

  case class Sensor(position: Coord, manhattanDistance: Int)
  case class Coord(x:Int, y:Int)
  val regex = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r

  def calcManhattanDistance(a: Coord, b: Coord) = {
    (a.x - b.x).abs + (a.y - b.y).abs
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
  }

  def isOccupied2(sensors: List[Sensor], line: Int, range: Range, beacons: List[Coord], predicate: (Sensor, Coord) => Boolean) = {
    val lineCoords = range.map(idx => Coord(idx, line)).toList.filter(coord => !beacons.contains(coord))
    val distanceBetweenSensorsAndLine = lineCoords
      .takeWhile(c => {
        var last = predicate(sensors.head,c)
        val e = sensors.drop(1).takeWhile(sensor => {
          val now = predicate(sensor,c)
          if (last != now) false else {
            last = now
            true
          }
        })
        val ef = e.size + 1 != sensors.size
        ef
      })

    if (lineCoords.size != distanceBetweenSensorsAndLine.size) {
      distanceBetweenSensorsAndLine.lastOption.map(e => Coord(e.x + 1, e.y))
    } else {
      None
    }

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
    //val occupied = isOccupied(positions, 200000, range, beacons, (s,l) => calcManhattanDistance(s.position, l) <= s.manhattanDistance)
    // .filter(list => list.exists(_._2)).map(_.head._1)
    //println(occupied.size)

    val range2 = (0 to 400000)
    val searchRange = (0 to 400000).takeWhile(line => {
      val f = isOccupied2(positions, line, range2, beacons, (s,l) => calcManhattanDistance(s.position, l) > s.manhattanDistance)
      if (f.isDefined) println((f.get.x * 400000) + f.get.y)
      true
    })


  }

}