package aoc2022

object day5 extends App {

  import io.io
  case class CrateStack(no: Integer, stack: List[String])
  case class Command(from: Int, to: Int, amount: Int)

  val regex = "move (\\d{1,2}) from (\\d{1,2}) to (\\d{1,2})".r

  def run(lines: List[String], is9001: Boolean) = {
    val stacks = (1 to 40 by 4)
      .map(idx => lines.filter(line => !line.contains("move") && line.nonEmpty)
        .map(line => { if (line.length - 1 < idx) { "" } else { String.valueOf(line(idx)) }})
        .filter(line =>  line.trim.nonEmpty)
      )
      .filter(_.nonEmpty)
      .map(list => CrateStack(Integer.parseInt(list.last), list.dropRight(1)))
      .toList

    val commands = lines.filter(line => line.contains("move"))
      .map {
        case regex(amount, from, to) => Command(Integer.parseInt(from), Integer.parseInt(to), Integer.parseInt(amount))
        case _ => throw new RuntimeException("Wrong regex")
      }

    val outcome = commands.foldLeft(stacks)((prev, command) => {
      val newOldStack = prev(command.from - 1).stack.drop(command.amount)
      val toMove = prev(command.from - 1).stack.slice(0, command.amount)
      val newNewStack = prev(command.to - 1).stack.prependedAll(if (is9001) { toMove } else { toMove.reverse })
      val e = prev.map(stack => {
        if (stack.no == command.from) { stack.copy(stack = newOldStack) }
        else if (stack.no == command.to) { stack.copy(stack = newNewStack) }
        else { stack }
      })
      e
    })

    outcome
  }

  io.load("aoc2022/day5") { lines =>
    println(run(lines, false).map(_.stack.head).mkString(""))
  }

  io.load("aoc2022/day5") { lines =>
    println(run(lines, true).map(_.stack.head).mkString(""))
  }

}