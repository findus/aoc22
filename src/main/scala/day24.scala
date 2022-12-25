import day22.Point

import scala.annotation.tailrec
import scala.collection.SortedMap

object day24 extends App {

  val blizzards = List('>','<','v','^')
  val orthogonal = Seq((1, 0), (-1, 0), (0, 1), (0, -1))
  val blizzardMovement = Map(
    '>' -> (1 , 0, 1),
    '<' -> (-1, 0, 1),
    '^' -> (0 ,-1, 1),
    'v' -> (0 , 1, 1)
  )

  case class Point(x: Int, y: Int, z: Int) {
    def delta(dx: Int, dy: Int, dz: Int) = Point(x + dx, y + dy, z + dz)
    def +(point: Point) = Point(x + point.x, y + point.y, z + point.z)
  }

  val ordered = new Ordering[Point] {
    override def compare(a: Point, b: Point): Int = if (a.y == b.y) a.x - b.x else b.y - a.y
  }

  io.load("day24") { lines =>
    val coords = SortedMap.from(lines.zipWithIndex.flatMap( { case(line,y) =>
      line.zipWithIndex.map { case (character,x) =>
        (Point(x,y,0),character)
      }
    }).toSet)(ordered)

    val blizzardPos = coords.filter(entry => blizzards.contains(entry._2)).toList
    val ground = coords.filter(entry => entry._2.equals('.')).keys.toList.concat(blizzardPos.map(_._1)).toSet
    val walls = coords.filter(entry => entry._2.equals('#')).keys.toSet
    val start = ground.minBy(_.y)
    val end = ground.maxBy(_.y)

    val maxX =  coords.keys.maxBy(_.x).x - 1
    val maxY = coords.keys.maxBy(_.y).y - 1
    println("Start")

    def bfs(start: Point, end: Point, points:Set[Point]): Int = {
      val todo = collection.mutable.Queue(start)
      val cost = collection.mutable.Map(start -> 0)

      while (todo.nonEmpty) {
        val cur = todo.dequeue()

        if (cur.x == end.x && cur.y == end.y) {
          return cost(cur)
        }

        val neighbours = Seq(
          Point(cur.x    , cur.y    , cur.z + 1),
          Point(cur.x - 1, cur.y    , cur.z + 1),
          Point(cur.x + 1, cur.y    , cur.z + 1),
          Point(cur.x    , cur.y - 1, cur.z + 1),
          Point(cur.x    , cur.y + 1, cur.z + 1),
        )

        neighbours.filterNot(cost.contains).filterNot(points.contains).foreach { next =>
          todo += next
          cost(next) = cost(cur) + 1
        }
      }
      -1
    }

    var (right, down, left, up, wall) = (blizzardPos.filter(_._2=='>').map(_._1),blizzardPos.filter(_._2=='v').map(_._1),blizzardPos.filter(_._2=='<').map(_._1),blizzardPos.filter(_._2=='^').map(_._1),walls.toList)
    wall = wall.appended(Point(1, -1, 0))
    val points = (right,left,up,down,wall)

    case class State(items:List[(List[Point],List[Point],List[Point],List[Point],List[Point])]) {
      def get() = items.flatMap(entry => entry._1 ++ entry._2 ++ entry._3 ++ entry._4 ++ entry._5)
    }
    def iterate(state: State): State = {
      State(state.items.appended(
        state.items.last._1.map(cur =>  Point((cur.x % maxX) + 1, cur.y, cur.z + 1)),
        state.items.last._2.map(cur => Point(((maxX + (cur.x - 2)) % maxX) + 1, cur.y, cur.z + 1)),
        state.items.last._3.map(cur => Point(cur.x, ((maxY + (cur.y - 2)) % maxY) + 1, cur.z + 1)),
        state.items.last._4.map(cur => Point(cur.x, (cur.y % maxY) + 1, cur.z + 1)),
        state.items.last._5.map(cur => Point(cur.x, cur.y, cur.z + 1))))
    }

    val iterator = Iterator.iterate(State(List(points)))(iterate)

    val allDimensions = iterator.drop(1000).next().get()

    val result = bfs(start,end,allDimensions.toSet)
    println(result)
    val result2 = bfs(end.copy(z=result),start,allDimensions.toSet)
    val result3 = bfs(start.copy(z=result2+result),end,allDimensions.toSet)
    println(result+result2+result3)

  }

}
