import day9.ActionType.ActionType

object day9 extends App {

  object ActionType extends Enumeration {
    type ActionType = Value
    val R = Value
    val U = Value
    val L = Value
    val D = Value

    def from(s: String) = {
      s match {
        case "R" => ActionType(R.id)
        case "U" => ActionType(U.id)
        case "L" => ActionType(L.id)
        case "D" => ActionType(D.id)
      }
    }

    def value(s: ActionType) = {
      s match {
        case R => ( 1, 0)
        case U => ( 0, 1)
        case L => (-1, 0)
        case D => ( 0,-1)
      }
    }
  }

  def addTuple(a: (Int,Int), b: (Int,Int)) = (a._1 + b._1, a._2 + b._2)

  def calcNewTPos(coordT: (Int, Int), newHpos: (Int, Int), preHpos: (Int,Int)) = {
    val newDistance = (newHpos._1 - coordT._1,newHpos._2 - coordT._2)
    (newDistance._1.abs, newDistance._2.abs) match {
      case (1,0) | (0,1) | (1,1) => coordT
      case (2,0) if newDistance._1 < 0 => (coordT._1 - 1, coordT._2)
      case (2,0) if newDistance._1 > 0 => (coordT._1 + 1, coordT._2)
      case (0,2) if newDistance._1 < 0 => (coordT._1                 , coordT._2 - 1)
      case (0,2) if newDistance._1 > 0 => (coordT._1                 , coordT._2 + 1)
      case (0,0) => newHpos
      case diagonal => preHpos
    }
  }

  type Coord = (Int,Int)
  case class Action(position: ActionType)
  case class State(coordH: Coord, coordT: Coord, vivistedT: List[Coord])

  def printState(newState: State) = {
    (0 to newState.coordT._2.max(newState.coordH._2)).reverse.foreach { y =>
      (0 to  newState.coordT._1.max(newState.coordH._1)).foreach { x =>
        if (newState.coordH.equals((x,y))) print("H")
        else if (newState.coordT.equals((x,y))) print("T")
        else print(".")
      }
      println("")
    }
    println("")
  }

  io.load("day9") { lines =>
    val actions = lines.flatMap(line => {
      val extrapolate = 0 until Integer.parseInt(line.split(" ")(1))
      extrapolate.map(_ => Action(ActionType.from(line.split(" ")(0))))
    })

    val initialState = State((0,0), (0,0), List.empty[Coord])
    val state = actions.foldLeft(initialState)((prev, action) => {
      val newHpos = addTuple(prev.coordH,ActionType.value(action.position))
      val newTpos = calcNewTPos(prev.coordT, newHpos, prev.coordH)
      val newState = State(newHpos, newTpos, prev.vivistedT.appended(newTpos))
      printState(newState)
      newState
    })
    val visits = state.vivistedT.distinct
    println(visits.size)
  }

  io.load("day9") { lines =>

  }
}
