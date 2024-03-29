package aoc2022

import scala.annotation.tailrec
import io.io

object day17 extends App {


  val range = 0 to 6

  val minusShape =
    """####
      |""".stripMargin

  val pipeShape =
    """#
      |#
      |#
      |#
      |""".stripMargin

  val plusShape =
    """ #
      |###
      | #
      |""".stripMargin

  val quadShape =
    """##
      |##
      |""".stripMargin

  val lShape =
    """  #
      |  #
      |###
      |""".stripMargin


  def asCoord(str: String) = {
    str.split("\n").zipWithIndex.flatMap(y => {
      y._1.zipWithIndex.filter(_._1.equals('#')).map(x => (x._2, y._2)).toList
    }).toSet
  }

  case class Rock(name: String,list: Set[(Int,Int)]) {
    def getHeight() = list.map(_._2).max + 1
  }

  case class RockPos(rock: Rock, position: (Int,Int)) {
    def getAbsolutePositions() = rock.list.map { case (x,y) => (x + position._1, position._2 -y) }
    def moveHorizontally(location: Char) = {
      copy( position = location match {
        case '>' => (this.position._1 + 1, this.position._2)
        case '<' => (this.position._1 - 1, this.position._2)
      })
    }
    def moveVertically = copy( position = (this.position._1, this.position._2 - 1))
  }

  case class State(rocks: List[RockPos], spawnHeight: Int, currentRock: RockPos, rockIter: Iterator[Rock], windIter: Iterator[Char]) {

    def collides(rock: RockPos): Boolean = {
      val absolutePosition = rock.getAbsolutePositions()

      if (absolutePosition.maxBy(_._1)._1 > 6 || absolutePosition.minBy(_._1)._1 < 0 || absolutePosition.minBy(_._2)._2 < 0 ) {
        return true
      }

      val allOtherPositions = rocks.flatMap(_.getAbsolutePositions()).toSet
      val collidingPoints = allOtherPositions.intersect(absolutePosition).size
      collidingPoints > 0
    }

    def highestPoint = {
      (rocks.maxBy(_.position._2).position._2) + 1
    }

    def draw(): Unit = {
      val positions = rocks.flatMap(_.getAbsolutePositions()).concat(currentRock.getAbsolutePositions().toList)
      val highestPoint = positions.maxBy(_._2)._2
      (0 to highestPoint).reverse.foreach { y => {
        (0 to 6).foreach { x =>
          if (positions.exists(e => e._1 == x && e._2 == y)) { print("#") } else { print(".") }
        }
        println("")
      }}
      println("")
    }
  }

  io.load("aoc2022/day17") { lines =>
    val mc = Rock("minus",asCoord(minusShape))
    val pc = Rock("plus",asCoord(plusShape))
    val lc = Rock("L",asCoord(lShape))
    val vc = Rock("pipe",asCoord(pipeShape))
    val qc = Rock("quad",asCoord(quadShape))

    @tailrec
    def fall(state: State, falling: Boolean ): State = {

      if (!falling) {
        return state
      }

      val wind = state.windIter.next()
      val newRockPos = state.currentRock.moveHorizontally(wind)
      val newPosBeforeFalling = if (state.collides(newRockPos)) state.currentRock else newRockPos
      val newPosAfterFalling = newPosBeforeFalling.moveVertically

      if (state.collides(newPosAfterFalling)) {
        val nextRock = state.rockIter.take(1).toList.head
        val newSpawnHeight = (state.copy(rocks = state.rocks.appended(newPosBeforeFalling)).highestPoint - 1) + nextRock.getHeight() + 3
        fall(state.copy(rocks = state.rocks.appended(newPosBeforeFalling), spawnHeight = newSpawnHeight, currentRock = RockPos(nextRock, (2,newSpawnHeight))), false)
      } else if (newPosAfterFalling.position._2 == 0) {
        val nextRock = state.rockIter.take(1).toList.head
        val newSpawnHeight = (state.copy(rocks = state.rocks.appended(newPosAfterFalling)).highestPoint - 1) + nextRock.getHeight() + 3
        fall(state.copy(rocks = state.rocks.appended(newPosAfterFalling), spawnHeight = newSpawnHeight, currentRock = RockPos(nextRock, (2,newSpawnHeight))), false)
      }else {
        fall(state.copy(currentRock = newPosAfterFalling), true)
      }
    }

    def round(state: State): State = {
      val st = fall(state, true)
     // st.draw()
      st
    }

    def iterate() = {
      val rockIterator = getRockIterator()
      val windIterator = getWindIterator()
      val next = rockIterator.next()
      val initHeight = 3 + next.getHeight()
      val rockPos = RockPos(next, (2,initHeight))

      val state = State(List.empty, initHeight, rockPos, rockIterator, windIterator)
      Iterator.iterate(state)(round)
    }

    def getRockIterator() = Iterator.continually(List(mc,pc,lc,vc,qc)).flatten

    def getWindIterator() = {
      val wind = lines.head.map(e => e).toList
      Iterator.continually(wind).flatten
    }

    val highest = iterate().drop(2022).next().highestPoint
    println(highest)

    (1000 to 3000).map(r => {

    })

  }

}
