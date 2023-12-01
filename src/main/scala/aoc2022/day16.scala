package aoc2022

import scala.annotation.tailrec
import io.io

object day16 extends App {


  def findShortestPath(start: String, end: String, graph: Map[String, Map[String, Int]]): NodePaths = {
    recurseFindShortestPath(NodePaths(start, end), graph)
  }

  @tailrec
  private def recurseFindShortestPath(nodePaths: NodePaths, graph: Map[String, Map[String, Int]]): NodePaths = {
    if (nodePaths.isFinished()) nodePaths
    else {
      val nextNode = nodePaths.nextNode(graph)
      recurseFindShortestPath(nextNode, graph)
    }
  }

  case class NodePaths(private val node: String, private val end: String, paths: Map[String, Int] = Map.empty) {

    def updatePath(entry: (String, Int)): NodePaths = {
      val currentDistance = paths.getOrElse(entry._1, Integer.MAX_VALUE)
      val newDistance = entry._2 + paths.getOrElse(node, 0)
      if (newDistance < currentDistance) {
        this.plus((entry._1, newDistance))
      } else
        this
    }

    def updatePaths(weightedPaths: Map[String, Map[String, Int]]): NodePaths
    = (weightedPaths.get(node).map(it => it.map(i => updatePath(i)).fold(copy())((acc, input) => acc.plus(input)))).getOrElse(copy()).minus(node)

    def nextNode(weightedPaths: Map[String, Map[String, Int]]): NodePaths = {
      val updatedPaths = updatePaths(weightedPaths)
      val nextNode = updatedPaths.paths.minByOption(_._2).map(_._1).get
      updatedPaths.copy(node = nextNode)
    }

    def isFinished() = node == end

    def plus(other: NodePaths) = copy(paths = paths.++(other.paths))

    def plus(p: (String, Int)) = copy(paths = paths.+(p))

    def minus(node: String) = copy(paths = paths - node)
  }

  case class Tunnel(name: String, flow: Int, paths: List[String])

  case class Turn(position: String, openValves: List[String], releasedPressure: Int, minutesPassed: Int, summedPressureRelease: Int)

  io.load("aoc2022/day16") { lines =>
    val regex = "Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)".r

    val valves = lines.map {
      case regex(from, amount, destinations) => Tunnel(from, Integer.parseInt(amount), destinations.split(",").map(_.trim).toList)
      case e => throw new Exception("Parsing failed")
    }

    val graph = valves.map(entry => (entry.name, entry.paths.map(e => (e, 1)).toMap)).toMap

    val points = valves.map(_.name)
    val allGraphDistances = points.map(point => (point, points.filter(entry => !entry.equals(point)))).flatMap { case (point, otherPoints) => otherPoints.map(op => {
      (point, findShortestPath(point, op, graph).paths.filter(_._1.equals(op)).head)
    })
    }.groupBy(_._1).map(entry => (entry._1, entry._2.map(_._2).toMap))

    val relevantValves = valves.filter(_.flow > 0)

    var iterations = 0;

    def goThrough(allGraphDistances: Map[String, Map[String, Int]], me: String, elephantStart: String, elephantTime: Int, time: Int, todo: Set[Tunnel], pressure: Int, edges: List[String]): Set[Int] = {
      val res = todo.flatMap(next => {
        val expiredTime = time + 1 + allGraphDistances(me)(next.name) // time to open valve + time to get there
        val extra = (30 - expiredTime) * valves.find(_.name.equals(next.name)).get.flow // amount of pressurerelease this valve will emit til the end
        println(edges.appended(next.name))
        if (expiredTime < 30) {
          //Next iteration
          iterations += 1
          Some(goThrough(allGraphDistances, next.name, elephantStart, elephantTime, expiredTime, todo - next, pressure + extra, edges.appended(next.name)))
        } else {
          None
        }
      })

      val res2 = todo.flatMap(next => {
        val expiredTime = elephantTime + 1 + allGraphDistances(elephantStart)(next.name) // time to open valve + time to get there
        val extra = (30 - expiredTime) * valves.find(_.name.equals(next.name)).get.flow // amount of pressurerelease this valve will emit til the end
        println(edges.appended(next.name))
        if (expiredTime < 30) {
          //Next iteration
          iterations += 1
          Some(goThrough(allGraphDistances, me, next.name, expiredTime, time, todo - next, pressure + extra, edges.appended(next.name)))
        } else {
          None
        }
      })

      res.flatten.+(pressure).toList.concat(res2.flatten).toSet
    }

    //idea from, because I was giga stuck: https://github.com/maneatingape/advent-of-code/blob/main/src/main/scala/AdventOfCode2022/Day16.scala
    def recursiveLookup(start: String,startElefant: String, timeElefant: Int, time: Int, todo: Set[Tunnel], pressure: Int, edges: List[String]): Int = {
      val res: _root_.scala.collection.immutable.Set[Int] = goThrough(allGraphDistances, start,startElefant, timeElefant, time, todo, pressure, edges)
      val r = res.foldLeft(pressure)((prev, action) => prev.max(action))
      r
    }

    val p1 = recursiveLookup("AA","AA", 0,30 , relevantValves.toSet, 0, List.empty)
    val p2 = recursiveLookup("AA","AA", 4,4 , relevantValves.toSet, 0, List.empty)
    println(p1)
    println(p2)
    println("Iterations:",iterations)

  }


}
