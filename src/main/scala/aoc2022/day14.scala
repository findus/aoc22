package aoc2022

import scala.util.control.Breaks.breakable
import io.io

object day14 extends App {

  def liesVertically(list: Array[(Int,Int)]) = list.head._2 != list.last._2

  trait State
  case class Falling(coord: Coordinate) extends State
  case class Resting(coordinate: Coordinate) extends State
  case class Endless(coordinate: Coordinate) extends State

  case class Coordinate(x: Int, y: Int)

  io.load("aoc2022/day14") { lines =>
    val rockStructures = lines.map(line => line.split(" -> ").map(str => (Integer.parseInt(str.split(",").head), Integer.parseInt(str.split(",").last))).sliding(2,1).toList).toList
    val rockCoordinates = rockStructures.map(structure => structure.foldLeft(List.empty[List[Coordinate]])((prev, structurePart) => {
      val vertically = liesVertically(structurePart)
      if (vertically) prev.appended((structurePart.minBy(_._2)._2 to structurePart.maxBy(_._2)._2).map(yPos => Coordinate(structurePart.head._1, yPos)).toList)
      else prev.appended((structurePart.minBy(_._1)._1 to structurePart.maxBy(_._1)._1).map(xPos => Coordinate(xPos, structurePart.head._2)).toList)
    }))
    val sandEntryPoint = Coordinate(500,0)
    var sandCoordinates = List.empty[Coordinate]
    val allRockCoordinates = rockCoordinates.flatten.flatten.distinct
    val bedrockYpos = allRockCoordinates.maxBy(_.y).y + 2
    val width = (1 to bedrockYpos).fold(1)((prev, action) => prev + 2 )
    val rangestart = 500 - ((width / 2) + 100)
    val rangeend = 500 + ((width / 2) + 100)
    val allRockCoordinatesB = allRockCoordinates.concat((rangestart to rangeend).map(x => Coordinate(x, bedrockYpos)).toList)

    def isFree(state: Coordinate, rocks: List[Coordinate]): Option[Coordinate] = {
      val isFree = !sandCoordinates.concat(rocks).contains(state)
      if (isFree) Some(state)
      else None
    }

    def simulate(state: Coordinate, rocks: List[Coordinate]) = {
      val newState = isFree(state.copy(y = state.y + 1), rocks).map(Falling)
      val left = isFree(state.copy(y = state.y + 1, x = state.x - 1), rocks).map(Falling)
      val right = isFree(state.copy(y = state.y + 1, x = state.x + 1), rocks).map(Falling)
      newState.getOrElse(left.getOrElse(right.getOrElse(Resting(state))))
    }

    def run(iterations: Int, rocks: List[Coordinate]): Unit = {
      for (x <- 0 to iterations) {
        breakable {
          var position = sandEntryPoint
          var state: State = Falling.apply(position)
          import util.control.Breaks._
          while (state.isInstanceOf[Falling]) {
            simulate(position, rocks) match {
              case Falling(coord) if coord.y > rocks.maxBy(_.y).y + 3 =>
                state = Endless(coord)
                ()
              case Falling(coord) =>
                position = coord
                ()
              case Resting(coord) =>
                sandCoordinates = sandCoordinates.appended(coord).toList
                state = Resting(coord)
                if (coord.y == 0) return ()
                break
            }
          }

        }
      }
    }


    run(1000, allRockCoordinates)
    val res = sandCoordinates.toList
    println(res.size)

    sandCoordinates = List.empty
    run(1000000, allRockCoordinatesB)
    val res2 = sandCoordinates.toList
    println(res2.size)

  }

  io.load("aoc2022/day14") { lines =>

  }
}
