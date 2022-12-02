
case class Round(enemyTurn: String, yourTurn: Response)
case class Response(whatType: String, count: Int)
case class OutCome(outcome: String, points: Int)

object day2 extends App {

  object Result extends Enumeration {
    val Win  = Value(6)
    val Draw = Value(3)
    val Loss = Value(0)
  }

  val ROCK = "Rock"
  val PAPER = "Paper"
  val SCISSOR = "Scissor"

  val rockResponse = Response(ROCK, 1)
  val paperResponse = Response(PAPER, 2)
  val scissorsResponse = Response(SCISSOR, 3)

  def getYourTurn(input: String) = {
    input match {
      case "X" => rockResponse
      case "Y" => paperResponse
      case "Z" => scissorsResponse
    }
  }

  def calcResult(enemyTurn: String) = {
    enemyTurn match {
      case ROCK =>
      case PAPER =>
      case SCISSOR =>

    }
  }

  def getEnemyTurn(input: String) = {
    input match {
      case "A" => ROCK
      case "B" => PAPER
      case "C" => SCISSOR
    }
  }

  def getResult(input: Round) = {
    val et = input.enemyTurn
    val yt = input.yourTurn.whatType
    if (et.equals(yt))                                 Result.Draw
    else if (yt.equals(ROCK)    && et.equals(PAPER))   Result.Loss
    else if (yt.equals(ROCK)    && et.equals(SCISSOR)) Result.Win
    else if (yt.equals(PAPER)   && et.equals(SCISSOR)) Result.Loss
    else if (yt.equals(PAPER)   && et.equals(ROCK))    Result.Win
    else if (yt.equals(SCISSOR) && et.equals(PAPER))   Result.Win
    else if (yt.equals(SCISSOR) && et.equals(ROCK))    Result.Loss
    else Result.Loss
  }

  io.load("day2") { lines =>

     val rounds = lines.map(line => {
       val split = line.split(" ")
       Round(getEnemyTurn(split.head), getYourTurn(split.last))
     })

    val e = rounds.map(round => (getResult(round), round.yourTurn.count))
    val results = e.map(ee => ee._2 + ee._1.id)
    println(results.sum)

  }

  def getNeededOutCome(input: String) = {
    input match {
      case "X" => Result.Loss
      case "Y" => Result.Draw
      case "Z" => Result.Win
    }
  }

  def getPoints(enemyTurn: String, neededOutCome: day2.Result.Value) = {
    enemyTurn match {
      case ROCK if neededOutCome == day2.Result.Loss => scissorsResponse
      case ROCK if neededOutCome == day2.Result.Draw => rockResponse
      case ROCK if neededOutCome == day2.Result.Win => paperResponse
      case PAPER if neededOutCome == day2.Result.Loss => rockResponse
      case PAPER if neededOutCome == day2.Result.Draw => paperResponse
      case PAPER if neededOutCome == day2.Result.Win => scissorsResponse
      case SCISSOR if neededOutCome == day2.Result.Loss => paperResponse
      case SCISSOR if neededOutCome == day2.Result.Win => rockResponse
      case SCISSOR if neededOutCome == day2.Result.Draw => scissorsResponse
    }
  }

  io.load("day2b") { lines =>

    val out = lines.foldLeft(0)((prev,line) => {
      val split = line.split(" ")
      val enemyTurn = getEnemyTurn(split.head)
      val neededOutcome = getNeededOutCome(split.last)
      val points = getPoints(enemyTurn, neededOutcome)
      prev + points.count + neededOutcome.id
    })

    println(out)
  }

}
