import scala.annotation.tailrec

object day12 extends App {

  private def getGraph(entries: Map[(Int,Int), Int]): Map[(Int, Int), Map[(Int, Int), Int]] = {
    entries.map(coord => {
      val neighbours = List((0, 1), (1, 0), (-1, 0), (0, -1))
      val neighs = neighbours.map(pos => (coord._1._1 + pos._1, coord._1._2 + pos._2))
        .map(coord => (coord, entries.get(coord)))
        .filter(_._2.isDefined)
        .filter(entry => (entry._2.get - entries(coord._1)) == 1 || (entry._2.get - entries(coord._1)) == 0 || (entry._2.get - entries(coord._1)) <= 0 )
        .map(entry => (entry._1, 1))
        .toMap
      (coord._1, neighs)
    })
  }

  def findShortestPath(start: (Int,Int), end: (Int,Int), graph: Map[(Int, Int), Map[(Int, Int), Int]]): NodePaths = {
    recurseFindShortestPath(NodePaths(start, end), graph)
  }

  @tailrec
  private def recurseFindShortestPath(nodePaths: NodePaths, graph: Map[(Int, Int), Map[(Int, Int), Int]]): NodePaths = {
    if (nodePaths.isFinished()) nodePaths
    else {
      val nextNode = nodePaths.nextNode(graph)
      recurseFindShortestPath(nextNode, graph)
    }
  }

  case class NodePaths(private val node: (Int,Int), private val end: (Int,Int), paths: Map[(Int,Int), Int] = Map.empty) {

    def updatePath(entry: ((Int,Int), Int)): NodePaths = {
      val currentDistance = paths.getOrElse(entry._1, Integer.MAX_VALUE)
      val newDistance = entry._2 + paths.getOrElse(node, 0)
      if (newDistance < currentDistance)
        this.plus((entry._1, newDistance))
      else
        this
    }

    def updatePaths(weightedPaths: Map[(Int,Int), Map[(Int,Int), Int]])
    = (weightedPaths.get(node).map(it => it.map(i => updatePath(i)).fold(copy())((acc,input) => acc.plus(input)))).getOrElse(copy()).minus(node)

    def nextNode(weightedPaths: Map[(Int,Int), Map[(Int,Int), Int]]): NodePaths = {
      val updatedPaths = updatePaths(weightedPaths)
      val nextNode = updatedPaths.paths.minByOption(_._2).map(_._1).get
      updatedPaths.copy(node = nextNode)
    }

    def isFinished() = node == end

    def plus(other: NodePaths) = copy(paths = paths.++(other.paths))

    def plus(p: ((Int,Int), Int)) = copy(paths = paths.+(p))

    def minus(node: (Int,Int))= copy(paths = paths - node)
  }

  private def getAlphabetIndex(column: (Char, Int)) = if (column._1 == 'S') -1 else if (column._1 == 'E') 26 else column._1.toInt - 97

  io.load("day12") { lines =>
    implicit val entries: Map[(Int, Int), Int] = lines.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(column => ((column._2, row._2), getAlphabetIndex(column)))).sorted.toMap
    val e = getGraph(entries)
    val end = entries.find(_._2 == 26).get
    val start = entries.find(_._2 == -1).get

    val eff = findShortestPath(end._1,(0,0),e)

    val firstPath = findShortestPath(start._1,end._1,e)
    println(firstPath.paths)
    println(firstPath.paths.get(end._1))

    val startPositions = entries.filter(entry => entry._2 == -1 || entry._2 == 0 ).toList
    println(startPositions.size)
    val test = startPositions.zipWithIndex.map(entry => {
      println(s"${entry._2 + 1} from ${startPositions.size}")
      (entry,findShortestPath(entry._1._1, end._1, e).paths.values.min)
    })
    println(test.map(_._2).min)
  }
}