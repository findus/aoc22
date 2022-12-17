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

  case class Rock(list: Set[(Int,Int)]) {
    def getHeight() = list.map(_._2).max + 1
  }

  case class RockPos(rock: Rock, position: (Int,Int)) {
    def getAbsolutePositions() = rock.list.map { case (x,y) => (x + position._1, position._2 -y) } //TODO y calculation wrong
    def moveHorizontally(location: Char) = {
      copy( position = location match {
        case '>' => (this.position._1 + 1, this.position._2)
        case '<' => (this.position._1 - 1, this.position._2)
      })
    }
    def moveVertically = copy( position = (this.position._1, this.position._2 - 1))
  }

  case class State(rocks: List[RockPos], spawnHeight: Int, currentRock: RockPos) {

    def collides(rock: RockPos): Boolean = {
      val absolutePosition = rock.getAbsolutePositions()

      if (absolutePosition.maxBy(_._1)._1 > 6 || absolutePosition.minBy(_._1)._1 < 0 || absolutePosition.minBy(_._2)._2 < 0 ) {
        return true
      }

      val allOtherPositions = rocks.flatMap(_.getAbsolutePositions()).toSet
      val collideingPoints = allOtherPositions.intersect(absolutePosition).size
      collideingPoints > 0
    }

    def highestPoint = {
      rocks.maxBy(_.position._2).position._2
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
    }
  }

  io.load("day17") { lines =>
    val mc = Rock(asCoord(minusShape))
    val pc = Rock(asCoord(plusShape))
    val lc = Rock(asCoord(lShape))
    val vc = Rock(asCoord(pipeShape))
    val qc = Rock(asCoord(quadShape))

    val roundRobin = Iterator.continually(List(mc,pc,lc,vc,qc)).flatten
    val wind = lines.head.map(e => e).toList
    val windRoundRobin = Iterator.continually(wind).flatten

    val next = roundRobin.next()
    val initHeight = 3 + next.getHeight()
    val rockPos = RockPos(next, (2,initHeight))
    val state = State(List.empty, initHeight, rockPos)

    val st = (1 to 2022).foldLeft(state)((prev, action) => {
      var falling = true
      var tempState = prev
      while (falling) {
        val wind = windRoundRobin.next()
        val newRockPos = tempState.currentRock.moveHorizontally(wind)
        val newPosBeforeFalling = if (tempState.collides(newRockPos)) tempState.currentRock else newRockPos
        val newPosAfterFalling = newPosBeforeFalling.moveVertically

        if (tempState.collides(newPosAfterFalling)) {
          val nextRock = roundRobin.take(1).toList.head
          val newSpawnHeight = tempState.copy(rocks = tempState.rocks.appended(newPosBeforeFalling)).highestPoint + nextRock.getHeight() + 3
          tempState = tempState.copy(rocks = tempState.rocks.appended(newPosBeforeFalling), spawnHeight = newSpawnHeight, currentRock = RockPos(nextRock, (2,newSpawnHeight)))
          falling = false
        } else if (newPosAfterFalling.position._2 == 0) {
          val nextRock = roundRobin.take(1).toList.head
          val newSpawnHeight = tempState.copy(rocks = tempState.rocks.appended(newPosAfterFalling)).highestPoint + nextRock.getHeight() + 3
          tempState = tempState.copy(rocks = tempState.rocks.appended(newPosAfterFalling), spawnHeight = newSpawnHeight, currentRock = RockPos(nextRock, (2,newSpawnHeight)))
          falling = false
        }else {
          tempState = tempState.copy(currentRock = newPosAfterFalling)
        }

      }

      //tempState.draw()
      //println("")

      tempState
    })

    println(st.highestPoint + 1)
  }

}
