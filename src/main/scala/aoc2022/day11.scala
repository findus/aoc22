package aoc2022

import scala.language.postfixOps
import io.io

object day11 extends App {

  case class Item(worryLevel: Long)
  case class Monkey(name: Int, items: List[Item], operator: String, riskToAdd: String, divisibleBy: Int , monkeys: Map[Boolean, Long], itemsInspected: Int)

  def generateStartingItems(monkeyInput: List[List[String]]) = {
    monkeyInput.map { lines =>
      val name = "Monkey (\\d):".r.findAllIn(lines.head).matchData.toList.head.group(1).toInt
      val items = lines(1).replace("Starting items: ","").trim.split(",").map(_.trim.toInt).toList
      val riskMathmaticalOperator = "Operation: new = old ([+*]) (\\d+|old)".r.findAllIn(lines(2)).matchData.toList.head.group(1)
      val riskToAdd = "Operation: new = old ([+*]) (\\d+|old)".r.findAllIn(lines(2)).matchData.toList.head.group(2)
      val divisibleBy = "Test: divisible by (\\d+)".r.findAllIn(lines(3).trim).matchData.toList.head.group(1)
      val ifTrue = "If true: throw to monkey (\\d+)".r.findAllIn(lines(4).trim).matchData.toList.head.group(1)
      val ifFalse = "If false: throw to monkey (\\d+)".r.findAllIn(lines(5).trim).matchData.toList.head.group(1)
      Monkey(name, items.map(_.toLong).map(Item.apply), riskMathmaticalOperator, riskToAdd, divisibleBy.toInt, List((true, ifTrue.toLong), (false, ifFalse.toLong)).toMap,0)
    }
  }

  private def simulate(monkeys: List[Monkey], range: Range, calculator: Monkey => List[(Item, Long)]) = {
    range.foldLeft(monkeys)((prev, round) => {
      val e = prev.indices.foldLeft(prev)((prev, index) => {
        val actions = calculateThrowingActions(prev(index), calculator)
        val thrown = prev.map(monkey => monkey.copy(items = monkey.items.appendedAll(actions.filter(_._2 == monkey.name).map(_._1))))
        thrown.map(m => if (m.name.equals(prev(index).name)) {
          prev(index).copy(items = List.empty, itemsInspected = prev(index).itemsInspected + actions.length)
        } else {
          m
        })
      })
      if (List(1,20,1000,2000).contains(round)) {
        println(e.map(_.itemsInspected))
      }
      e
    })
  }

  private def calculateThrowingActions(monkey: Monkey, calculator: Monkey => List[(Item, Long)]) = {
    calculator(monkey)
  }

  private def p1c: Monkey => List[(Item, Long)] = monkey => {
    monkey.items.map { item =>
      val newRisk = monkey.operator match {
        case "*" if monkey.riskToAdd.equals("old") => item.worryLevel * item.worryLevel
        case "*" => item.worryLevel * monkey.riskToAdd.toInt
        case "+" if monkey.riskToAdd.equals("old") => item.worryLevel + item.worryLevel
        case "+" => item.worryLevel + monkey.riskToAdd.toInt
      }
      val boredRiskLevel = (newRisk / 3).floor.toLong
      (item.copy(boredRiskLevel), monkey.monkeys(boredRiskLevel % monkey.divisibleBy == 0))
    }
  }

  io.load("aoc2022/day11") { lines =>
    val monkeyInput = lines.sliding(7,7).toList
    val monkeys = generateStartingItems(monkeyInput)
    println(simulate(monkeys, 1 to 20, p1c).map(_.itemsInspected).sorted.reverse.slice(0,2).product)
  }

  io.load("aoc2022/day11") { lines =>
    val monkeyInput = lines.sliding(7,7).toList
    val monkeys = generateStartingItems(monkeyInput)

    def p2c: Monkey => List[(Item, Long)] = monkey => {

      val mod = monkeys.map(_.divisibleBy).product

      monkey.items.map { item =>
        val newRisk = monkey.operator match {
          case "*" if monkey.riskToAdd.equals("old") => (item.worryLevel * item.worryLevel) % mod
          case "*" => (item.worryLevel * monkey.riskToAdd.toInt) % mod
          case "+" if monkey.riskToAdd.equals("old") => (item.worryLevel + item.worryLevel) % mod
          case "+" => (item.worryLevel + monkey.riskToAdd.toInt) % mod
        }
        val boredRiskLevel = newRisk
        (item.copy(boredRiskLevel), monkey.monkeys(boredRiskLevel % monkey.divisibleBy == 0))
      }
    }

    val s = simulate(monkeys, 1 to 10000, p2c)
    println(s.map(_.itemsInspected).sorted.reverse.slice(0,2).map(BigInt.apply).product)
  }
}
