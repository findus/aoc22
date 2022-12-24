import day22.Point

import scala.annotation.tailrec
import scala.collection.SortedMap

object day24 extends App {

  val blizzards = List('>','<','v','^')
  val orthogonal = Seq((1, 0), (-1, 0), (0, 1), (0, -1))
  val blizzardMovement = Map(
    '>' -> (1 , 0),
    '<' -> (-1, 0),
    '^' -> (0 ,-1),
    'v' -> (0 , 1)
  )

  case class Point(x: Int, y: Int) extends Ordering[Point] with Ordered[Point] {
    override def compare(a: Point, b: Point): Int = if (a.y == b.y) a.x - b.x else b.y - a.y
    override def compare(that: Point): Int = compare(this,that)
    def adjacent: Seq[Point] = orthogonal.map(e => this.delta(e._1,e._2))
    def delta(dx: Int, dy: Int) = Point(x + dx, y + dy)
    def +(point: Point) = Point(x + point.x, y + point.y)
  }

  def bfs(grid: Set[Point], blizzards: List[Point], start: Point, end: Point): Int = {
    val cost = collection.mutable.Map(start -> 0)
    val todo = collection.mutable.Queue(start)

    while (todo.nonEmpty) {
      val current = todo.dequeue()
      if (current == end) return cost(current)

      val nextCost = cost(current) + 1
      current.adjacent.filter(grid.contains).foreach { next =>
        if (!cost.contains(next) || nextCost < cost(next))
          cost(next) = nextCost
        todo.enqueue(next)
      }
    }
    -1
  }

  val ordered = new Ordering[Point] {
    override def compare(a: Point, b: Point): Int = if (a.y == b.y) a.x - b.x else b.y - a.y
  }

  io.load("day24") { lines =>
    val coords = SortedMap.from(lines.zipWithIndex.flatMap( { case(line,y) =>
      line.zipWithIndex.map { case (character,x) =>
        (Point(x,y),character)
      }
    }).toSet)(ordered)

    val blizzardPos = coords.filter(entry => blizzards.contains(entry._2)).toList
    val ground = coords.filter(entry => entry._2.equals('.')).keys.toList.concat(blizzardPos.map(_._1)).toSet
    val walls = coords.filter(entry => entry._2.equals('#')).keys.toSet
    val start = ground.minBy(_.y)
    val end = ground.maxBy(_.y)

    val maxX = ground.maxBy(_.x)
    val maxY = ground.maxBy(_.y)

    def draw(grid: List[Point], blizzards: List[(Point,Char)]) = {
      lines.zipWithIndex.foreach { case(line,y) =>
        (line.zipWithIndex.foreach) { case(_,x) =>
          if (walls.contains(Point(x,y))) print("#")
          if (ground.contains(Point(x,y)) && !blizzards.map(_._1).contains(Point(x,y))) print(".")
          if (blizzards.map(_._1).contains(Point(x,y))) print(blizzards.find(e => e._1.equals(Point(x,y))).get._2)
        }
        println("")
      }
      println("")
    }

    def newBlizzardPos(point: Point, direction: Char) = {
      val (x,y) = blizzardMovement(direction)
      val newPoint = point + Point(x,y)
      val newBlizzardPosition = if (walls.contains(newPoint)) {
        direction match {
          case '>' => Point(1         , newPoint.y)
          case '<' => Point(maxX.x    , newPoint.y)
          case '^' => Point(newPoint.x, maxY.y    )
          case 'v' => Point(newPoint.x, 1         )
        }
      } else newPoint
      newBlizzardPosition
    }

    case class State(position: Point, blizzards:  List[(Point,Char)], walls: Set[Point], steps: Int)

    def simulate(state: State): State = {

      val newBlizzards = state.blizzards.map { case(p,d) => (newBlizzardPos(p,d),d) }

//      val next = state.position.adjacent.filter(entry => !walls.contains(entry) && !newBlizzards.map(_._1).contains(entry) && entry.y > 0 )
//      draw(coords.keys.toList, newBlizzards)
//      val s = next.map(next => simulate(State(next, newBlizzards, walls, state.steps + 1)))
//      s.minBy(_.steps)
    }

    val result = simulate(State(start, blizzardPos, walls, 0))



    println(coords)
  }

}
