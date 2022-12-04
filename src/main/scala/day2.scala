
case class Round(enemyTurn: String, yourTurn: Response)
case class Response(whatType: String, count: Int)
case class OutCome(outcome: String, points: Int)

object day2 extends App {

  def getNumber(input: String) = {
    input match {
      case "A" | "X" => 1   //Rock     //Lose
      case "B" | "Y" => 2   //Paper    //Draw
      case "C" | "Z" => 3   //Scissors //Win
    }
  }

  def winner(p1: Int, p2: Int): Int = {
    val result = Math.floorMod(p1 - p2, 3)
    if (result == 2) { 6 } else if (result == 0) { 3 } else { 0 }
  }

  io.load("day2") { lines =>
     val rounds = lines.foldLeft(0)((prev, line) => {
       val split = line.split(" ")
       prev + winner(getNumber(split.head), getNumber(split.last)) +  getNumber(split.last)
     })

    println(rounds)
  }

  io.load("day2") { lines =>

    val rounds = lines.foldLeft(0)((prev, line) => {
      val (a,b) = (line.split(" ").head, line.split(" ").last)
      b match {
        case "X" => prev + (Math.floorMod((getNumber(a) - 1) - 1, 3) + 1)
        case "Y" => prev + 3 + (getNumber(a) - 1) + 1
        case "Z" => prev + (Math.floorMod((getNumber(a) - 1) + 1, 3) + 1) + 6
      }
    })

    println(rounds)
  }

}
