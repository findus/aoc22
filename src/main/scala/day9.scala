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

  def calcNewTPos(coordT: (Int, Int), newHpos: (Int, Int)) = {
    val newDistance = (newHpos._1 - coordT._1, newHpos._2 - coordT._2)
    (newDistance._1.abs, newDistance._2.abs) match {
      case (1,0) | (0,1) | (1,1) => coordT
      case (2,0) if newDistance._1 < 0 => (coordT._1 - 1, coordT._2)
      case (2,0) if newDistance._1 > 0 => (coordT._1 + 1, coordT._2)
      case (0,2) if newDistance._2 < 0 => (coordT._1                 , coordT._2 - 1)
      case (0,2) if newDistance._2 > 0 => (coordT._1                 , coordT._2 + 1)
      case (0,0) => newHpos
      case _ => newDistance match {
        case ( 1, 2) | ( 2, 1) | ( 2, 2)  => addTuple(coordT,( 1, 1))
        case ( 1,-2) | ( 2,-1) | ( 2,-2)  => addTuple(coordT,( 1,-1))
        case (-1, 2) | (-2, 1) | (-2, 2)  => addTuple(coordT,(-1, 1))
        case (-1,-2) | (-2,-1) | (-2,-2)  => addTuple(coordT,(-1,-1))
      }
    }
  }

  type Coord = (Int,Int)
  case class Action(position: ActionType)
  case class State(name: String, coord: Coord, visited: List[Coord])

  private def simulate(actions: List[Action], start: List[State]) = {
    val state = actions.foldLeft(start)((prev1, action) => {
      val newHpos =   prev1.head.copy(coord = addTuple(prev1.head.coord, ActionType.value(action.position)))
      prev1.drop(1).foldLeft(List(newHpos))((prev,input) => {
        val newTpos = calcNewTPos(input.coord, prev.last.coord)
        prev.appended(State(input.name, newTpos, input.visited.appended(newTpos)))
      })
    })
    val visits = state.last.visited.distinct
    println(visits.size)
  }

  private def parseActions(lines: List[String]) = {
    val actions = lines.flatMap(line => {
      val extrapolate = 0 until Integer.parseInt(line.split(" ")(1))
      extrapolate.map(_ => Action(ActionType.from(line.split(" ")(0))))
    })
    actions
  }

  io.load("day9") { lines =>
    val actions: List[day9.Action] = parseActions(lines)
    val initialStateH = State("H", (0,0), List.empty[Coord])
    val initialStateT = State("T", (0,0), List.empty[Coord])
    val start = List(initialStateH, initialStateT)
    simulate(actions, start)
  }

  io.load("day9") { lines =>
    val actions: List[day9.Action] = parseActions(lines)
    val initialStateH = State("H", (0,0), List.empty[Coord])
    val rope = (1 to 9).map(nr => State(nr.toString, (0,0), List.empty[Coord])).toList.prepended(initialStateH)
    simulate(actions, rope)
  }
}
