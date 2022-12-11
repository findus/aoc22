import scala.language.postfixOps

object day11 extends App {

  case class Item(worryLevel: Int)
  case class Monkey(name: Int, items: List[Item], operator: String, riskToAdd: String, divisibleBy: Int , monkeys: Map[Boolean, Int], itemsInspected: Int)

  def generateStartingItems(monkeyInput: List[List[String]]) = {
    monkeyInput.map { lines =>
      val name = "Monkey (\\d):".r.findAllIn(lines.head).matchData.toList.head.group(1).toInt
      val items = lines(1).replace("Starting items: ","").trim.split(",").map(_.trim.toInt).toList
      val riskMathmaticalOperator = "Operation: new = old ([+*]) (\\d+|old)".r.findAllIn(lines(2)).matchData.toList.head.group(1)
      val riskToAdd = "Operation: new = old ([+*]) (\\d+|old)".r.findAllIn(lines(2)).matchData.toList.head.group(2)
      val divisibleBy = "Test: divisible by (\\d+)".r.findAllIn(lines(3).trim).matchData.toList.head.group(1)
      val ifTrue = "If true: throw to monkey (\\d+)".r.findAllIn(lines(4).trim).matchData.toList.head.group(1)
      val ifFalse = "If false: throw to monkey (\\d+)".r.findAllIn(lines(5).trim).matchData.toList.head.group(1)
      Monkey(name, items.map(Item.apply), riskMathmaticalOperator, riskToAdd, divisibleBy.toInt, List((true, ifTrue.toInt), (false, ifFalse.toInt)).toMap,0)
    }
  }

  io.load("day11") { lines =>
    val monkeyInput = lines.sliding(7,7).toList
    val monkeys = generateStartingItems(monkeyInput)

    val afterRounds = (1 to 20).foldLeft(monkeys)((prev, action) => {
      val monkeys = prev.indices.foldLeft(prev)((prev, index) => {
        val actions = calculateThrowingActions(prev(index))
        val thrown = prev.map(monkey => monkey.copy(items = monkey.items.appendedAll(actions.filter(_._2 == monkey.name).map(_._1))))
        val ne = thrown.map(m => if (m.name.equals(prev(index).name)) { prev(index).copy(items = List.empty, itemsInspected = prev(index).itemsInspected + actions.length) } else { m })
        ne
      })
      monkeys
    })

    println(afterRounds.map(_.itemsInspected).sorted.reverse.slice(0,2).product)

  }

  private def calculateThrowingActions(monkey: Monkey) = {
    monkey.items.map { item =>
      val newRisk = monkey.operator match {
        case "*" if monkey.riskToAdd.equals("old") => item.worryLevel * item.worryLevel
        case "*" => item.worryLevel * monkey.riskToAdd.toInt
        case "+" if monkey.riskToAdd.equals("old") => item.worryLevel + item.worryLevel
        case "+" => item.worryLevel + monkey.riskToAdd.toInt
      }
      val boredRiskLevel = (newRisk / 3).floor.toInt
      (item.copy(boredRiskLevel), monkey.monkeys(boredRiskLevel % monkey.divisibleBy == 0))
    }
  }

  io.load("day11") { lines =>

  }
}
