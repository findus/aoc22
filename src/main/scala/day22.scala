import day22.{Point, State}

import scala.collection.SortedMap

class Simulator(grid: SortedMap[Point, Char], startsX: List[Int], endsX: List[Int], startsY: List[Int], endsY: List[Int]) {
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
      case "S" => (0, 1)
      case "E" => (1, 0)
      case "N" => (0, -1)
    }

    val newPos = Point(point.x + direction._1, point.y + direction._2)
    newPos match {
      case x if !shouldWrap(x) && wouldBeStuck(x) => point
      case x if shouldWrap(x) => wrap(facing, point)
      case x if !shouldWrap(x) && !wouldBeStuck(x) => x
    }
  }
  def simulate(state: State, instruction: (Int,Char)): State = {
    val newState = (1 to instruction._1).foldLeft(state.position)((prev, _) => move(state.facing, prev))
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
}
class CubeSimulator(grid: SortedMap[Point, Char], startsX: List[Int], endsX: List[Int], startsY: List[Int], endsY: List[Int]) extends Simulator(grid: SortedMap[Point, Char], startsX: List[Int], endsX: List[Int], startsY: List[Int], endsY: List[Int]) {

  def stuck(newPos: Point): Option[Point] = {
    if (wouldBeStuck(newPos)) None else Some(newPos)
  }

  override def simulate(state: State, instruction: (Int, Char)): State = {

    val newState = (1 to instruction._1).foldLeft((state.position,state.facing))((prev, _) => {
      val newPosition = prev._2 match {
        case "E" => Point(prev._1.x + 1, prev._1.y    )
        case "W" => Point(prev._1.x - 1, prev._1.y    )
        case "N" => Point(prev._1.x    , prev._1.y - 1)
        case "S" => Point(prev._1.x    , prev._1.y + 1)
      }

      val chunkSize = 4
      val yChunkCount = 3
      val xChunkCount = 4
      val currentChunk = Point(prev._1.x / chunkSize % xChunkCount,prev._1.y / chunkSize % yChunkCount)
      val nextChunk    = Point(newPosition.x / chunkSize % xChunkCount,newPosition.y / chunkSize % yChunkCount)
      val posInChunk   = Point(newPosition.x % xChunkCount, newPosition.y % yChunkCount)
      val xOffset = prev._1.y % chunkSize
      val yOffset = prev._1.x % chunkSize
      val e = (currentChunk,nextChunk) match {
        // 4 to 6
        case (Point(2,1),Point(3,1)) => (stuck(Point(3*chunkSize + ((chunkSize - 1) - xOffset ),2*chunkSize)), "S")
        // 5 to 1
        case (Point(2,2),Point(2,0)) => (stuck(Point(0*chunkSize + ((chunkSize - 1) - yOffset ),(2*chunkSize - 1))), "N")
        // 3 to 1
        case (Point(1,1),Point(1,0)) => (stuck(Point(2*chunkSize, 0*chunkSize + yOffset)), "E")
        case _  =>  (stuck(newPosition), prev._2)
      }
      val d = e._1.map(issome => (issome,e._2)).getOrElse((prev._1,prev._2))
      d
    })

    val newFacing = newState match {
      case f if f._2.equals("N") && instruction._2.equals('L') => "W"
      case f if f._2.equals("E") && instruction._2.equals('L') => "N"
      case f if f._2.equals("S") && instruction._2.equals('L') => "E"
      case f if f._2.equals("W") && instruction._2.equals('L') => "S"
      case f if f._2.equals("N") && instruction._2.equals('R') => "E"
      case f if f._2.equals("E") && instruction._2.equals('R') => "S"
      case f if f._2.equals("S") && instruction._2.equals('R') => "W"
      case f if f._2.equals("W") && instruction._2.equals('R') => "N"
      case e => e._2
    }

    val s = State(newState._1, newFacing)
    s
  }
}

object day22 extends App {

  val regex1 = "(\\d+[A-Z])".r
  val regex2 = "(\\d+)".r

  case class State(position: Point, facing: String)
  case class Point(x: Int, y: Int) extends Ordering[Point] with Ordered[Point] {
    override def compare(a: Point, b: Point): Int = if (a.y == b.y) a.x - b.x else b.y - a.y
    override def compare(that: Point): Int = compare(this,that)
  }

  val ordered = new Ordering[Point] {
    override def compare(a: Point, b: Point): Int = if (a.y == b.y) a.x - b.x else b.y - a.y
  }

  private def parse(lines: List[String]) = {
    val instructions =
      regex1.findAllIn(lines.last).matchData.toList.map(_.toString).map { case line =>
        val facing = line.last
        val amount = Integer.parseInt(line.dropRight(1))
        (amount, facing)
      }.appended(regex2.findAllMatchIn(lines.last).map(_.start).toList.last , 'C')

    val linesWithoutInstructions = lines.dropRight(1).filter(_.isEmpty == false)
    val maxX = linesWithoutInstructions.maxBy(_.length).length
    val maxY = linesWithoutInstructions.length
    val paddedList = linesWithoutInstructions.map(_.padTo(maxX, ' '))
    val transposed = paddedList.transpose.map(e => e.mkString(""))

    val seq =
      (0 until maxY).flatMap { case y =>
        (0 to maxX).map { case x =>
          val character = if (linesWithoutInstructions(y).length > x) {
            linesWithoutInstructions(y)(x)
          } else {
            ' '
          }
          (Point(x, y), character)
        }
      }

    val startsX = linesWithoutInstructions.map(line => "[#.]".r.findFirstMatchIn(line).map(_.start).get)
    val endsX = linesWithoutInstructions.map(line => "[#.]".r.findAllMatchIn(line).map(_.start).toList.last)

    val startsY = transposed.map(line => "[#.]".r.findFirstMatchIn(line).map(_.start).get)
    val endsY = transposed.map(line => "[#.]".r.findAllMatchIn(line).map(_.start).toList.last)
    val grid = SortedMap.from(seq)(ordered)

    val start = Point(linesWithoutInstructions.head.indexOf("."), 0)
    val state = State(start, "E")

    val facingval = (state: State) => state.facing  match {
      case "N" => 3
      case "W" => 2
      case "E" => 0
      case "S" => 1
    }

    (instructions, startsX, endsX, startsY, endsY, grid, state, facingval)
  }

  io.load("day22") { lines =>
    val (instructions: List[(Int, Char)], startsX: List[Int], endsX: List[Int], startsY: List[Int], endsY: List[Int], grid: SortedMap[Point, Char], state: State, facing: (State => Int)) = parse(lines)
    val simulator = new Simulator(grid, startsX, endsX, startsY, endsY)
    val finalState = instructions.foldLeft(state)((prev, action) => simulator.simulate(prev, action))

    val row = finalState.position.y + 1
    val column = finalState.position.x + 1

    val result = (1000 * row + 4 * column + facing(state))
    println(result)
  }

  io.load("day22") { lines =>
    val (instructions: List[(Int, Char)], startsX: List[Int], endsX: List[Int], startsY: List[Int], endsY: List[Int], grid: SortedMap[Point, Char], state: State, facing: (State => Int)) = parse(lines)
    val simulator = new CubeSimulator(grid, startsX, endsX, startsY, endsY)
    val finalState = instructions.foldLeft(state)((prev, action) => simulator.simulate(prev, action))

    val row = finalState.position.y + 1
    val column = finalState.position.x + 1

    val result = (1000 * row + 4 * column + facing(finalState))
    println(result)
  }
}
