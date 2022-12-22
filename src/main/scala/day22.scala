import scala.collection.SortedMap

object day22 extends App {

  val regex1 = "(\\d+[A-Z])".r
  case class State(position: Point, facing: String)
  case class Point(x: Int, y: Int) extends Ordering[Point] with Ordered[Point] {
    override def compare(a: Point, b: Point): Int = if (a.y == b.y) a.x - b.x else b.y - a.y
    override def compare(that: Point): Int = compare(this,that)
  }

  val ordered = new Ordering[Point] {
    override def compare(a: Point, b: Point): Int = if (a.y == b.y) a.x - b.x else b.y - a.y
  }

  def simulate(state: State, instruction: (Int,Char), grid: SortedMap[Point, Char], lines: List[String], startsX: List[Int], endsX: List[Int], startsY: List[Int], endsY: List[Int]): State = {
    def wouldBeStuck(position: Point) = {
      grid(position).equals('#')
    }
    def shouldWrap(position: Point) = {
      position.y < 0 || position.y >= grid.map(_._1.y).max + 1 || position.x < 0 || position.x >= grid.map(_._1.x).max || grid(position) == ' '
    }
    def wrap(direction: String, oldPosition: Point): Point = {
      val newPos = direction match {
        case "E" => Point(startsX(oldPosition.y), oldPosition.y)
        case "W" => Point(endsX(oldPosition.y), oldPosition.y)
        case "N" => Point(oldPosition.x, endsY(oldPosition.x))
        case "S" => Point(oldPosition.x, startsY(oldPosition.x))
      }
      if (wouldBeStuck(newPos)) oldPosition else newPos
    }
    def move(facing: String, point: Point) = {
      val direction = facing match {
        case "W" => (-1, 0)
        case "S" => ( 0, 1)
        case "E" => ( 1, 0)
        case "N" => ( 0,-1)
      }

      val newPos = Point(point.x + direction._1, point.y + direction._2)
       newPos match {
        case x if !shouldWrap(x) && wouldBeStuck(x) => point
        case x if shouldWrap(x) => wrap(facing,point)
        case x if !shouldWrap(x) && !wouldBeStuck(x) => x
       }
    }

    val newState = (1 to instruction._1).foldLeft(state.position)((prev,_) => move(state.facing, prev))

    State(newState, instruction._2 match {
      case f if state.facing.equals("W") && f.equals('L') => "S"
      case f if state.facing.equals("W") && f.equals('R') => "N"
      case f if state.facing.equals("N") && f.equals('L') => "W"
      case f if state.facing.equals("N") && f.equals('R') => "E"
      case f if state.facing.equals("E") && f.equals('L') => "N"
      case f if state.facing.equals("E") && f.equals('R') => "S"
      case f if state.facing.equals("S") && f.equals('L') => "E"
      case f if state.facing.equals("S") && f.equals('R') => "W"
      case _ => state.facing
    })
  }

  io.load("day22") { lines =>
    val instructions =
      regex1.findAllIn(lines.last).matchData.toList.map(_.toString).map { case line =>
      val facing = line.last
      val amount = Integer.parseInt(line.dropRight(1))
        (amount, facing)
    }.appended((Integer.parseInt(lines.last.substring(lines.last.length - 2)),'C'))


    val linesWithoutInstructions = lines.dropRight(1).filter(_.isEmpty == false)
    val maxX = linesWithoutInstructions.maxBy(_.length).length
    val maxY = linesWithoutInstructions.length
    val paddedList = linesWithoutInstructions.map(_.padTo(maxX,' '))
    val transposed = paddedList.transpose.map(e => e.mkString(""))

    val seq =
      (0 until maxY).flatMap { case y =>
        (0 to maxX).map { case x =>
          val character = if (linesWithoutInstructions(y).length > x) {
            linesWithoutInstructions(y)(x) match {
              case ' ' => ' '
              case x   => x
            }
          } else {
            ' '
          }
          (Point(x, y), character)
        }
      }

    val startsX = linesWithoutInstructions.map( line => "[#.]".r.findFirstMatchIn(line).map(_.start).get)
    val endsX = linesWithoutInstructions.map( line => "[#.]".r.findAllMatchIn(line).map(_.start).toList.last)

    val startsY = transposed.map( line => "[#.]".r.findFirstMatchIn(line).map(_.start).get)
    val endsY = transposed.map( line => "[#.]".r.findAllMatchIn(line).map(_.start).toList.last)
    val grid = SortedMap.from(seq)(ordered)

    val start = Point(linesWithoutInstructions.head.indexOf("."),0)
    val state = State(start, "E")

    val finalState = instructions.foldLeft(state)((prev, action) => simulate(prev, action, grid, linesWithoutInstructions, startsX, endsX, startsY, endsY))

    val facingval = finalState.facing match {
      case "N" => 3
      case "W" => 2
      case "E" => 0
      case "S" => 1
    }

    val row = finalState.position.y + 1
    val column = finalState.position.x + 1

    val result = (1000 * row + 4 * column + facingval)
    println(result)
  }
}
