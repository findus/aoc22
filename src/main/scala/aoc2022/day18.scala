package aoc2022

import io.io

object day18 extends App {

  case class Cube(x: Int, y: Int, z: Int) {
    def unConnectedSides(list: Set[Cube]) = {
      val amount = neighbours.count { c => list.contains(c) }
      (6 - amount)
    }
    def neighbours = {
      List((-1,0,0),(0,0,-1),(0,-1,0),( 1,0,0),(0,0, 1),(0, 1,0)).map { case (dx,dy,dz) => Cube(x+dx, y+dy, z+dz) }
    }
  }

  def getAirPockets(lava: Set[Cube]) = {

    val ymax = lava.maxBy(_.y).y + 1
    val xmax = lava.maxBy(_.x).x + 1
    val zmax = lava.maxBy(_.z).z + 1

    val allcubesPlusOuter = (-1 to ymax).flatMap(y => (-1 to xmax).flatMap(x => (-1 to zmax).map(z => Cube(x, y, z)))).toSet

    val start = Cube(-1,-1,-1)

    val todo = collection.mutable.Stack(start)
    val visitedNonLavaBlocks = collection.mutable.Set(start)

    while (todo.nonEmpty) {
      todo.pop().neighbours
        .filterNot(lava.contains)
        .filterNot(visitedNonLavaBlocks.contains)
        .foreach { next =>
          if (allcubesPlusOuter.contains(next)) {
            todo.push(next)
            visitedNonLavaBlocks += next
          }
        }
    }
    val e = lava.toList.map(lavaStone => {
      val neighbours = lavaStone.neighbours
      neighbours.count(neighbour => visitedNonLavaBlocks.contains(neighbour))
    })
    val b = e.sum
    b
    //e.count(e => e == 0)
  }

  io.load("aoc2022/day18") { lines =>
    val cubes = lines.map(entry => {
      val coords = entry.split(",")
      Cube(Integer.parseInt(coords(0)), Integer.parseInt(coords(1)), Integer.parseInt(coords(2)))
    }).toSet

    val e = cubes.toList.map(c => c.unConnectedSides(cubes)).sum
    println(e)

    val airPockets = getAirPockets(cubes)
    println(airPockets)
  }

}
