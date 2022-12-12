import scala.annotation.tailrec

object day12 extends App {

  private def getNeighbours(coord: (Int,Int), entries: Map[(Int,Int), Int], visited:List[(Int,Int)]) = {
    val e = List((0, 1), (1, 0), (-1, 0), (0, -1))
      .map(pos => (coord._1 + pos._1, coord._2 + pos._2))
      .filter(entry => !visited.contains(entry))
      .map(coord => (coord,entries.get(coord)))
      .filter(_._2.isDefined)
      .map(entry => (entry._1, entry._2.get))
      .filter(entry => (entry._2 - entries(coord)) == 1 ||  (entry._2 - entries(coord)) == 0)
    e
  }

  private def getShortestPath(coord: (Int,Int), entries: Map[(Int,Int), Int], steps: List[(Int,Int)], visited: List[(Int,Int)]): Int = {
    if (entries(coord) == 26) {
      steps.size
    } else {
      val neighbours = getNeighbours(coord, entries, visited)
      val visitedCoords = visited.appended(coord)
      val e = neighbours.map(entry => getShortestPath(entry._1, entries, steps.appended(entry._1), visitedCoords))
      e.filter(e => e > 0).minOption.getOrElse(-1)
    }
  }

  private def getAlphabetIndex(column: (Char, Int)) = if (column._1 == 'S') -1 else if (column._1 == 'E') 26 else column._1.toInt - 97

  io.load("day12") { lines =>
    implicit val entries: Map[(Int, Int), Int] = lines.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(column => ((column._2, row._2), getAlphabetIndex(column)))).sorted.toMap
    val e = getShortestPath((0,0) , entries, List.empty, List.empty)
    println(e)
  }

  io.load("day12") { lines =>

  }
}