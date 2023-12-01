package aoc2022
import io.io

object day21 extends App {

  val regex1 = "([a-z]{4}): ([a-z]{4}) ([+-/*]) ([a-z]{4})".r
  val regex2 = "([a-z]{4}): (\\d+)".r

  def solve(monkeh: Monkeh, map: Map[String, Monkeh]): Double = {
    monkeh match {
      case m: NumberMonkeh => m.nr
      case d: DependentMonkeh =>
        val x1 = solve(map(d.x1), map)
        val x2 = solve(map(d.x2), map)
        d.operant match {
          case "*" => x1 * x2
          case "-" => x1 - x2
          case "+" => x1 + x2
          case "/" => x1 / x2
        }
    }
  }

  def printEquation(monkeh: Monkeh, map: Map[String, Monkeh]): String = {
    monkeh match {
      case m: NumberMonkeh => s"${m.nr}"
      case d: DependentMonkeh => s"(${printEquation(map(d.x1), map)} ${d.operant} ${printEquation(map(d.x2), map)})"
    }
  }

  private def parseMonkehs(lines: List[String]) = {
    lines.map {
      case regex1(mname, x1, operant, x2) => DependentMonkeh(mname, x1, x2, operant)
      case regex2(mname, x1) => NumberMonkeh(mname, Integer.parseInt(x1))
    }
  }

  private def genMonkeysWithMeNr(lines: List[String], nr: Double) = {
    parseMonkehs(lines)
      .map {
        case me if me.name.equals("humn") => me.asInstanceOf[NumberMonkeh].copy(nr = nr)
        case smelse => smelse
      }
      .map(m => (m.name, m)).toMap
  }

  trait Monkeh { def name: String }
  case class NumberMonkeh(name: String, nr: Double) extends Monkeh
  case class DependentMonkeh(name: String, x1: String, x2: String, operant: String) extends Monkeh

  io.load("aoc2022/day21") { lines =>
    val monkehs = parseMonkehs(lines)
    val map = monkehs.map(m => (m.name, m)).toMap
    println(printEquation(monkehs.filter(_.name.equals("root")).head, map))
    println(solve(monkehs.filter(_.name.equals("root")).head, map))
  }

  io.load("aoc2022/day21") { lines =>
    val monkehs = parseMonkehs(lines)
    val tuple = lines.find(_.contains("root")).map { case regex1(_, x1, _, x2) => (x1, x2) }.head

    val root1 = tuple._1
    val root2 = tuple._2

    val m1 = monkehs.map(m => (m.name, m)).toMap

    var min = 400000000000L
    var max = 4000000000000L

    val nrToSearch = solve(monkehs.filter(_.name.equals(root2)).head, m1)
    var solved = false
    var resultVal = -1L
    while (!solved) {
      val middle = (min + max) / 2
      val map2 = genMonkeysWithMeNr(lines, middle)
      val result = solve(monkehs.filter(_.name.equals(root1)).head, map2)
      if (result < nrToSearch) {
        max = middle - 1
      }
      else if (result > nrToSearch) {
        min = middle + 1
      }
      else {
        solved = true
        println("a:", nrToSearch)
        println("b:", result)
        println("my number:", middle)
      }
      resultVal = middle
    }
    println(resultVal)

  }

}
