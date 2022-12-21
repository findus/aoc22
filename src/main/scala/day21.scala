import scala.collection.mutable

object  day21 extends App {

  val regex1 = "([a-z]{4}): ([a-z]{4}) ([+-/*]) ([a-z]{4})".r
  val regex2 = "([a-z]{4}): (\\d+)".r

  def solve(monkeh: Monkeh, map: Map[String,Monkeh]): Long = {
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

  trait Monkeh {
    def name: String
  }
  case class NumberMonkeh(name: String, nr: Long) extends Monkeh
  case class DependentMonkeh(name: String, x1: String, x2: String, operant: String) extends Monkeh

  io.load("day21") { lines =>
    val monkehs = lines.map {
      case regex1(mname, x1, operant, x2) => DependentMonkeh(mname, x1, x2, operant)
      case regex2(mname, x1) => NumberMonkeh(mname, Integer.parseInt(x1))
    }

    val map = monkehs.map(m => (m.name, m)).toMap
    val test = solve(monkehs.filter(_.name.equals("root")).head, map)
    println(test)
  }

}
