package aoc2022
import io.io

object  day23 extends App {

  val adjacentPositions = Map(
    ("N", Point( 0,-1)),
    ("NW",Point(-1,-1)),
    ("W", Point(-1, 0)),
    ("SW",Point(-1, 1)),
    ("S", Point( 0, 1)),
    ("SE",Point( 1, 1)),
    ("E", Point( 1, 0)),
    ("NE",Point( 1,-1)),
  )

  def getDirections(directions: List[String]) = {
    adjacentPositions.filter( entry => directions.contains(entry._1))
  }

  def printgrid(grid: Map[Point, Char], newPositions: List[Point]) = {
    val x = grid.keys.maxBy(_.x).x
    val y = grid.keys.maxBy(_.y).y
    (0 to y).foreach { y =>
      (0 to x).foreach { x =>
        if (newPositions.contains(Point(x,y))) print("#") else print(".")
      }
      println("")
    }
    println("")
  }

  var globalGrid: Map[Point,Char] = null
  var gridMaxY: Int = 0
  var gridMaxX: Int = 0

  case class Point(x: Int, y: Int) {
    def +(point: Point) = Point(x + point.x, y + point.y)
    def alone(grid: Set[Point]): Boolean = {
      val adjPos = adjacentPositions.map(_._2+(this))
      adjPos.nonEmpty && adjPos.count(pos => grid.contains(pos)) == 0
    }
    def alone(grid: Set[Point], subset: Map[String, Point]): Boolean = {
      val adjPos = subset.map(_._2+(this))
      val e = adjPos.nonEmpty &&  adjPos.count(pos => grid.contains(pos)) == 0
      e
    }
  }

  io.load("aoc2022/day23") { lines =>

    globalGrid =
      lines.indices.flatMap { case y =>
        (0 until lines(y).length).map { case x =>
          val character = lines(y)(x)
          (Point(x, y), character)
        }
      }.toMap

    gridMaxY = globalGrid.map(_._1.y).max
    gridMaxX = globalGrid.map(_._1.x).max

    val (elvePositions, _) = globalGrid.partition(_._2.equals('#'))

    val north = (point: Point, positions: Set[Point]) => if (point.alone(positions, getDirections(List("N","NE","NW")))) Some((point,point+(Point( 0,-1)),"NORD")) else None
    val south = (point: Point, positions: Set[Point]) => if (point.alone(positions, getDirections(List("S","SE","SW")))) Some((point,point+(Point( 0, 1)),"SÜD")) else None
    val west = (point: Point, positions: Set[Point]) => if (point.alone(positions, getDirections(List("W","NW","SW")))) Some((point,point+(Point(-1, 0)),"WEST")) else None
    val east = (point: Point, positions: Set[Point]) => if (point.alone(positions, getDirections(List("E","NE","SE")))) Some((point,point+(Point( 1, 0)),"OST")) else None
    val proposalList = List(north,south,west,east)

    case class State(items: Set[Point], proposals: List[(Point,Set[Point]) => Option[(Point,Point, String)]], same: Boolean)

    def round(state: State): State = {
      //first half
      val (loneElves,nonLoneElves) = state.items.partition(pos => pos.alone(state.items))
      val moveProposals = nonLoneElves.flatMap(point => {
        val results = state.proposals.map(func => func(point, nonLoneElves)).filter(_.isDefined)
        if (results.nonEmpty) results.head else None
      })

      //2nd half
      // only if one elve moves to the new location it moves, if 2 would end up on the same coord do nothing
      val filteredProposals = moveProposals.filter(entry => moveProposals.count(e => e._2.equals(entry._2)) == 1).toList

      val newPositions = loneElves ++ nonLoneElves.map(oldEntry => if (filteredProposals.map(_._1).contains(oldEntry)) {
        filteredProposals.find(p => p._1.equals(oldEntry)).get._2
      } else {
        oldEntry
      }
      )

      val e = state.proposals.head
      val d = state.proposals.drop(1).appended(e)
      State(newPositions, d, newPositions.equals(state.items))
    }

    def iterate = {
      val state = State(elvePositions.keys.toSet, proposalList, false)
      Iterator.iterate(state)(round)
    }


    //P1
    val state = iterate.drop(10).next()
    val grid = state.items
    val minX = grid.minBy(_.x).x
    val maxX = grid.maxBy(_.x).x
    val minY = grid.minBy(_.y).y
    val maxY = grid.maxBy(_.y).y
    val smolRect = (minY to maxY).flatMap(y => (minX to maxX).map(x => Point(x,y))).toSet
    val res = smolRect -- grid
    println(res.size)

    //P2
    val idx = iterate.indexWhere(_.same == true)
    println(idx)
  }
}
