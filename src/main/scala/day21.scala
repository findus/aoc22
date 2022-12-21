import scala.collection.mutable

object  day21 extends App {

  val regex1 = "([a-z]{4}): ([a-z]{4}) ([+-/*]) ([a-z]{4})".r
  val regex2 = "([a-z]{4}): (\\d+)".r

  def getValue: (String,String,String,String, mutable.Map[String, Long]) => Option[Long] = {
    (name,x,op,y,map) => {
      val mx = map.get(x)
      val my = map.get(y)
      if (mx.isEmpty && my.isEmpty)
        Option.empty[Long]
      else {
        val value = op match {
          case "*" => Some((mx.get * my.get).toLong)
          case "-" => Some((mx.get - my.get).toLong)
          case "+" => Some((mx.get + my.get).toLong)
          case "/" => Some((mx.get / my.get).toLong)
        }
        map.put(name,value.get)
        value
      }
    }
  }

  trait Monkeh
  case class NumberMonkeh(name: String, nr: Long, map: mutable.Map[String, Long]) extends Monkeh
  case class DependentMonkeh(name: String, get: () => Option[Long], map: mutable.Map[String, Long]) extends Monkeh

  io.load("day21") { lines =>
    val valueCache = collection.mutable.Map.empty[String,Long]
    val monkehs = lines.map {
      case regex1(mname, x1, operant, x2) => DependentMonkeh(mname, () => getValue(mname,x1, operant, x2, valueCache), valueCache)
      case regex2(mname, x1) => NumberMonkeh(mname, Integer.parseInt(x1), valueCache)
    }

    println(monkehs)
    monkehs.map {
      case monkeh1: NumberMonkeh =>
        valueCache.put(monkeh1.name, monkeh1.nr)
        monkeh1
      case monkeh2 => monkeh2
    }
      .map {

      }


  }

}
